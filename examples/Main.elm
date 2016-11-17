import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Navigation
import Navigation.Router as Router
import Navigation.Builder as Builder


main : Program Never Model Msg
main =
  Navigation.program LocationChanged
    { init = init
    , view = view
    , update = update
    , subscriptions = (\_ -> Sub.none)
    }



-- MODEL


{- This example shows how to maintain application state, like the "counter"
and also store the location history. We also must keep the state of the
router (locations that are created from wihin our program).
-}
type alias Model =
  { counter : Int
  , history : List Navigation.Location
  , router : Router.Model
  }


initModel : Navigation.Location -> Model
initModel location =
    { counter = 0
    , history = [ location ]
    , router = Router.init location
    }


init : Navigation.Location -> ( Model, Cmd Msg )
init location =
  ( initModel location, Cmd.none )



-- UPDATE


type Msg
  = LocationChanged Navigation.Location
  | Increment
  | Decrement
  | SetCount Int


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        -- Location changed message.
        LocationChanged location ->
            let
                ( newRouter, external ) =
                    Router.locationChanged model.router location

                newModel =
                    { model
                    | history = location :: model.history
                    , router = newRouter
                    }
            in
                Router.processLocation external
                    update location2messages location newModel []

        -- Non-location messages that may change model state
        -- call a sub-function that returns a new model with
        -- a router updated from the `Router.urlChanged` function.
        _ ->
            let
                ( newModel, cmd ) = updateModelState msg model
                ( newRouter, routerCmd ) =
                    Router.urlChanged model.router (delta2url model newModel)
            in
                ( { newModel | router = newRouter }
                , Cmd.batch [ cmd, routerCmd ]
                )


{-| This function handles just the non-router parts of our model,
and does not emit any commands.
-}
updateModelState : Msg -> Model -> (Model, Cmd Msg)
updateModelState msg model =
    case msg of
        Increment ->
            ( { model | counter = model.counter + 1 }, Cmd.none )

        Decrement ->
            ( { model | counter = model.counter - 1 }, Cmd.none )

        SetCount counter ->
            ( { model | counter = counter }, Cmd.none )

        _ ->
            -- other messages (including LocationChanged) that
            -- don't affect state of non-router parts of our model
            ( model, Cmd.none )


{-| `delta2url` will be called when your model changes. The first parameter is
the model's previous value, and the second is the model's new value.

Your function should return a `Just Router.UrlChange` if a new URL should be
displayed in the browser's location bar (or `Nothing` if no change to the URL
is needed). This library will check the current URL before setting a new one,
so you need not worry about setting duplicate URLs -- that will be
automatically avoided.

The reason we provide both the previous and current model for your
consideration is that sometimes you may want to do something differently
depending on the nature of the change in the model, not just the new value.
For instance, it might make the difference between using `NewEntry` or
`ModifyEntry` to make the change.

Note that this function will *not* be called when processing the
`LocationChanged` message.
-}
delta2url : Model -> Model -> Maybe Router.UrlChange
delta2url _ current =
    -- We're using a `Builder` to build up the possible change. You don't
    -- have to do that ... you can construct a `UrlChange` however you like.
    --
    -- So, as the last step, we map our possible `Builder` to a `UrlChange`.
    let
        pathBuilder = Builder.builder
            |> Builder.replacePath [ toString current.counter ]
    in
        Just <| Builder.toHashChange pathBuilder



{-|`location2messages` will be called when a change in the browser's URL is
detected, either because the user followed a link, typed something in the
location bar, or used the back or forward buttons.

We use the `Navigation.Builder` module to help us parse the location, but
you could also use another package like
[`evancz/url-parser`](http://package.elm-lang.org/packages/evancz/url-parser/latest)
to parse the path or hash into nicely structured Elm values, and then create
a list of state-changing messages from those values.

Note that this function will *not* be called when your `delta2url` method
initiates a `UrlChange` -- since in that case, the relevant change in the
model has already occurred.

Your function should return a list of messages that your `update` function
can respond to. Those messages will be fed into your app, to produce the
changes to the model that the new URL implies.
-}
location2messages : Navigation.Location -> List Msg
location2messages location =
    let
        path = Builder.path <| Builder.fromHash location.href
    in
        case path of
            first :: _ ->
                case String.toInt first of
                    Ok value ->
                        [ SetCount value ]

                    Err _ ->
                        -- If it wasn't an integer, then no action ... we could
                        -- show an error instead, of course.
                        []

            _ ->
                -- If nothing provided for this part of the URL, return empty list
                []



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Counter" ]
        , button [ onClick Decrement ] [ text "-" ]
        , div [ countStyle ] [ text (toString model.counter) ]
        , button [ onClick Increment ] [ text "+" ]
        , h1 [] [ text "Pages" ]
        , ul [] (List.map viewLink [ "bears", "cats", "dogs", "elephants", "42", "fish" ])
        , h1 [] [ text "History" ]
        , ul [] (List.map viewLocation model.history)
        ]


countStyle : Attribute any
countStyle =
    style
        [ ( "font-size", "20px" )
        , ( "font-family", "monospace" )
        , ( "display", "inline-block" )
        , ( "width", "50px" )
        , ( "text-align", "center" )
        ]


viewLink : String -> Html Msg
viewLink name =
    li [] [ a [ href ("#!/" ++ name) ] [ text name ] ]


viewLocation : Navigation.Location -> Html Msg
viewLocation location =
    li [] [ a [ href location.hash ] [ text (location.pathname ++ location.hash) ] ]
