module Navigation.Router exposing
    ( Model
    , UrlChange
    , HistoryEntry(..)
    , init
    , locationChanged
    , processLocation
    , urlChanged
    )

{-| This module provides routing for single-page apps based on changes to the
the browser's location. To use this module, create a `Navigation.program`
or `Navigation.programWithFlags` in your main module, and follow the
protocols described here to pass state changes (that can possibly change
the URL shown in your browser's location bar), and location changes
(that can either come from your program changing the location, or from
a user typing a new URL into the location bar, following a link, etc.).

Routing happens in both directions
-- that is, changes to the browser's location are translated to messages
your app can respond to, and changes to your app's state are translated to
changes in the browser's location. The net effect is to make it possible for
the 'back' and 'forward' buttons in the browser to do useful things, and for
the state of your app to be partially bookmark-able.

The router state and protocols in this module are responsible for updating
the browser's history and prevent infinite loops that can be could be caused
if you are both changing the location and responding to location changes.

See the documentation on the
[`elm-lang/navigation` package](http://package.elm-lang.org/packages/elm-lang/navigation/latest)
for a basic discussion of how location changes are passed to a
`Navigation.program` and how you set up the program to create a `LocationChanged`
message (Note: `LocationChanged` is just a message tag you create; you could
name it anything you like).

For a discussion of the
differences between the official module and this one, see the
[package documentation](http://package.elm-lang.org/packages/pzingg/elm-navgiation-extra/latest).

# Router State

Your program is responsible for maintaining the state of the `router`, by
keeping a `Navigation.Router.Model` member in your program's larger model.
This state is used to detect whether incoming changes to your program's location
are "internal" or "external".  Call the `Navigation.Router.init` function
as part of the `init` function in your `Navigation.program`.

@docs Model, init

# Processing Location Changes

Your program's `update` function must update the router's state when
handling the `LocationChanged` message, by calling
`Navigation.Router.locationChanged`.  After updating your program's model,
the router can process any additional commands that should be performed
when the URL changes, by calling `Navigation.Router.processLocation`.

@docs locationChanged, processLocation

# Setting Urls Based on Program State

When any messages other than a 'LocationChanged' message are received
by your program's `update` function, you may want changes to your model
to be reflected in a new URL.  You do this by generating a
`Navigation.Router.UrlChange` type that indicates what the new URL should
be and whether the URL should replace or modify the browser's current history.

Your program's `update` function calls `Navigation.Router.urlChanged` to
let the router remember the URL that you are setting, so that when the
location change is received later, an infinite loop is prevented.

@docs UrlChange, HistoryEntry, urlChanged
-}

import Dict exposing (Dict)
import Erl
import Navigation



-- SUPPORTING TYPES


{-| Indicates a change to be made in the URL, either creating
a new entry in the browser's history (`NewEntry`), or merely replacing the
current URL (`ModifyEntry`).

This is ultimately implemented via
[`Navigation.newUrl`](http://package.elm-lang.org/packages/elm-lang/navigation/1.0.0/Navigation#newUrl) or
[`Navigation.modifyUrl`](http://package.elm-lang.org/packages/elm-lang/navigation/1.0.0/Navigation#modifyUrl).
The reason we use this intermediate type is so that we can check whether the
provided string already corresponds to the current URL. In that case, we can
avoid creating a spurious duplicate entry in the browser's history.

The reason we take a `String` (rather than a more structured type) is that
there may be several ways you might want to build up the required URL. We
don't want to be prescriptive about that. However, the `String` you provide
must follow a couple of rules.

* The `String` must already be uri-encoded.

* The `String` must either start with a '/', a `?' or a '#'.

    * If it starts with a '/', it will be interpreted as a full path, including
      optional query parameters and hash.

    * If it starts with a '?', then we'll assume that you want the current
      path to stay the same -- only the query parameters and hash will change.

    * If it starts with a '#', then we'll assume that you want the current
      path and query parameters (if any) to stay the same -- only the
      hash will change.

So, what you should *not* provide is the scheme, host, or authentication
method -- that is, no "http://elm-lang.org". You should also not use relative
URLs. (Let me know if you'd like relative URLs -- we might be able to do
something sensible with them, but we don't yet in this version).

One way to construct a `UrlChange` in a modular way is to use the
`RouteUrl.Builder` module. However, a variety of approaches are possible.
-}
type alias UrlChange =
    { entry : HistoryEntry
    , url : String
    }


{-| Indicates whether to create a new entry in the browser's history, or merely
modify the current entry.

One could have used a `Bool` for this instead, but I hate remembering what
`True` actually means.
-}
type HistoryEntry
    = NewEntry
    | ModifyEntry



{-| Encapsulates the router's state.  A `Navigation.Router.Model` must be
included in your program's larger model.

`reportedUrl` is the last Url reported to us via urlUpdate.

`expectedUrlUpdates` represents how many outstanding commands we've
sent to change the URL. We increment it when we send a command, and
decrement it when `urlUpdate` is called (unless it's already zero,
of course).
-}
type alias Model =
    { reportedUrl : Erl.Url
    , expectedUrlUpdates : Int
    }


{-| Initalize the router's `Model` with the initial location passed to
your program at startup.
-}
init : Navigation.Location -> Model
init location =
    { reportedUrl = Erl.parse location.href
    , expectedUrlUpdates = 0
    }


{-| Updates the router's `Model` when a location message has been
received in the program's `update` function.

Parameters:

* `model` is the current state of your program's `Navigation.Router.Model`
(not the larger model of the program).

* `location` is the `Navigation.Location` value received through your
program's `LocationChanged` message.

The function returns a `Tuple` consisting of the updated `Navigation.Router.Model`
and a `Bool` flag, `external`, indicating if the change was from the "outside",
i.e. from the user clicking on a link, typing in the location bar,
or following a bookmark.

This is Step 1 of procssing a location message in the `update` function.

Step 2 is to update your program's larger model, keeping the new state of the
router's model.

Step 3 is to call `Navigation.Router.processLocation` with the `external`
flag returned here, with your program's `update` and `location2messages`
functions, the location and your program's model.

Here's an example `update` implementation showing the three steps
to handle the `LocationChanged` message:

```
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        -- Location changed message
        LocationChanged location ->
            let
                -- Step 1
                ( newRouter, external ) =
                    Router.locationChanged model.router location

                -- Step 2
                newModel =
                    { model
                    | history = location :: model.history
                    , router = newRouter
                    }
            in
                -- Step 3
                Router.processLocation external
                    update location2messages location newModel []

        -- Non-location messages (see documentation below)...
```
-}
locationChanged : Model -> Navigation.Location -> ( Model, Bool )
locationChanged model location =
    let
        ( nextUpdates, external ) =
            if model.expectedUrlUpdates > 0 then
                -- This is a location change that we were expecting, because we did
                -- it in response to a change in the app's state.  So, we don't
                -- make any *further* change to the app's state here ... we
                -- just record that we've seen the urlUpdate we expected.
                ( model.expectedUrlUpdates - 1, False )
            else
                -- This is an href which came from the outside ... i.e. clicking on a link,
                -- typing in the location bar, following a bookmark. So, we need to update
                -- the app's state to correspond to the new location.
                ( 0, True )

        newModel =
            { reportedUrl = Erl.parse location.href
            , expectedUrlUpdates = nextUpdates
            }
    in
        ( newModel, external )


{-| If the location change came from an outside source, this function
calls the program's `location2messages` function which generates a list
of program messages.  Each message is processed by calling back to the
program's `update` function (possibly updating the program's larger model
and possibly creating additional commands).

If the location change came from an internally generated URL (created by
your program's `delta2url` function), this function just returns the
model and commands passed to it, without calling the `update` function.

Your program must implement the `location2messages` function.
It should return a list of messages that your `update` function
can respond to. Those messages will be fed into your app, to produce the
changes to the model that the new URL implies.

Here's an example implementation for `location2messages`:

```
route : UrlParser.Parser (Msg -> a) a
route =
    UrlParser.map SetCount (UrlParser.s "!" </> UrlParser.int)


location2messages : Navigation.Location -> List Msg
location2messages location =
    let
        maybeMsg = UrlParser.parseHash route location
    in
        case maybeMsg of
            Just msg ->
                [ msg ]

            Nothing ->
                []
```

Parameters for `processLocation`:

* `external` is a Bool flag (as returned from `Navigation.Router.locationChanged`)
indicating whether this is an "outside" location change.

* `update` is your program's `update` function (which will be called recursively
to process messages generated by your `location2messages` function).

* `location2messages` is your program's `location2messages` function. See the
[package documentation](http://package.elm-lang.org/packages/pzingg/elm-navgiation-extra/latest)
for a description of how your implementation should be developed.

* `location` is the `Navigation.Location` record that will be passed to
`location2messages` to generate state-changing messages.

* `model` is your program's larger model (that will be updated when the
messages are processed).

* `commands` are any other commands that should be batched when
your `update` function returns.

The function returns the possibly updated model and list of commands, so
you can use the value of this function as the return value
of the `LocationChanged` branch of your program's `update` function.
-}
processLocation : Bool -> (msg -> model -> ( model, Cmd msg ))
    -> (Navigation.Location -> List msg)
    -> Navigation.Location -> model -> List (Cmd msg)
    -> ( model, Cmd msg )
processLocation external update location2messages location model commands =
    if external then
        let
            step msg ( mdl, xs ) =
                case update msg mdl of
                    ( stepModel, stepCmd ) ->
                        ( stepModel, stepCmd :: xs )

            ( newModel, moreCommands ) =
                List.foldl step ( model, [] ) (location2messages location)
        in
            newModel ! (commands ++ moreCommands)
    else
        model ! commands


{-| To process a change of your program's state, first create a function
`delta2url` in your program.

Then in your program's `update` function, if your model changes,
call `delta2url` with two parameters. The first parameter is
the model's previous value, and the second is the model's new value.

Your `delta2url` function should return a `Just Navigation.Router.UrlChange`
if a new URL should be displayed in the browser's location bar;
or `Nothing` if no change to the URL is needed.

The reason we provide both the previous and current model for your
consideration is that sometimes you may want to do something differently
depending on the nature of the change in the model, not just the new value.
For instance, it might make the difference between using `NewEntry` or
`ModifyEntry` to make the change.

Here's an example implementation of a `delta2url` function, where
we just put our model's `counter` value into a URL hash:

```
delta2url : Model -> Model -> Maybe UrlChange
delta2url _ current =
    Just <|
        { entry = NewEntry
        , url = "#!/" ++ (toString current.counter)
        }
```

Note: Do not call `delta2url` when processing a `LocationChanged` message.
Instead use the `Navigation.Router.locationChanged` and
`Navigation.Router.processLocation` functions described above.

After calling `delta2url`, step two of processing a state change that
may result in a new URL to be added to your browser's location bar is
to then call this function, `Navigation.Router.urlChanged`.

The `urlChanged` function will check the `urlChange` parameter against
the value of the last URL received by the router before setting a new one,
so you need not worry about setting duplicate URLs -- that will be
automatically avoided.

Parameters for `urlChanged`:

* `model` is the current state of your program's `Navigation.Router.Model`
(not the larger model of the program).

* `urlChange`, is the `Maybe Navigation.Router.UrlChange` value
you received from your program's `delta2url` function.

The function returns a `Tuple`, containing the new router state
and possibly a `Cmd` from the elm-lang `Navigation` module
(either `Navigation.newUrl` or `Navigation.modifUrl`).

After calling this function, update the `router` component of your program's
model with the new router state, and return the command from your
program's `update` function.

Here's the rest of the `update` function
(and a sub-function `updateModelState` that is used for common processing)
from the example program:

```elm
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        -- Location changed message
        LocationChanged location ->
        -- See documentation above...

        -- Non-location messages
        _ ->
            let
                ( newModel, cmd, mightChangeUrl ) = updateModelState msg model
            in
                if mightChangeUrl then
                    let
                        ( newRouter, routerCmd ) =
                            Router.urlChanged model.router (delta2url model newModel)
                    in
                        ( { newModel | router = newRouter }
                        , Cmd.batch [ cmd, routerCmd ]
                        )
                else
                    ( newModel, cmd )


updateModelState : Msg -> Model -> ( Model, Cmd Msg, Bool )
updateModelState msg model =
    case msg of
        Increment ->
            ( { model | counter = model.counter + 1 }, Cmd.none, True )

        Decrement ->
            ( { model | counter = model.counter - 1 }, Cmd.none, True )

        SetCount counter ->
            ( { model | counter = counter }, Cmd.none, True )

        _ ->
            ( model, Cmd.none, False )
```
-}
urlChanged : Model -> Maybe UrlChange -> ( Model, Cmd msg )
urlChanged model urlChange =
    let
        maybeUrlChange =
            urlChange
                |> Maybe.map (normalizeUrl model.reportedUrl)
                |> Maybe.andThen (checkDistinctUrl model.reportedUrl)
    in
        case maybeUrlChange of
            Just urlChange ->
                ( { reportedUrl = Erl.parse urlChange.url
                  , expectedUrlUpdates = model.expectedUrlUpdates + 1
                  }
                , urlChange2Cmd urlChange
                )

            Nothing ->
                ( model, Cmd.none )



-- IMPLEMENTATION
-- Interprets the UrlChange as a Cmd


urlChange2Cmd : UrlChange -> Cmd msg
urlChange2Cmd change =
    change.url
        |> case change.entry of
            NewEntry ->
                Navigation.newUrl

            ModifyEntry ->
                Navigation.modifyUrl


mapUrl : (String -> String) -> UrlChange -> UrlChange
mapUrl func c1 =
    { c1 | url = func c1.url }



-- Whether one Url is equal to another, for our purposes (that is, just comparing
-- the things we care about).


eqUrl : Erl.Url -> Erl.Url -> Bool
eqUrl u1 u2 =
    u1.path
        == u2.path
        && u1.hasTrailingSlash
        == u2.hasTrailingSlash
        && u1.hash
        == u2.hash
        && (Dict.toList u1.query)
        == (Dict.toList u2.query)


checkDistinctUrl : Erl.Url -> UrlChange -> Maybe UrlChange
checkDistinctUrl old new =
    if eqUrl (Erl.parse new.url) old then
        Nothing
    else
        Just new


url2path : Erl.Url -> String
url2path url =
    "/"
        ++ (String.join "/" url.path)
        ++ if url.hasTrailingSlash && not (List.isEmpty url.path) then
            "/"
           else
            ""



-- Supplies the default path or query string, if needed


normalizeUrl : Erl.Url -> UrlChange -> UrlChange
normalizeUrl old change =
    mapUrl
        (if String.startsWith "?" change.url then
            \url -> url2path old ++ url
         else if String.startsWith "#" change.url then
            \url -> url2path old ++ Erl.queryToString old ++ url
         else
            \url -> url
        )
        change
