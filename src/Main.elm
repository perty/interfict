module Main exposing (decodeStory, main)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Json.Decode as Decode


main : Program () Model Message
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type alias Model =
    { count : Int }


type alias Story =
    { scene : List Scene
    }


type alias Scene =
    { home : String
    , name : String
    , route : List SceneOption
    }


type alias SceneOption =
    { optionText : String
    , target : String
    }


init : flags -> ( Model, Cmd message )
init _ =
    ( { count = 0 }, Cmd.none )


type Message
    = Increment
    | Decrement


update : Message -> Model -> ( Model, Cmd Message )
update message model =
    -- First we consider which message we have received.
    case message of
        -- The increment message! Let's increment. We create a copy of the model with count that is one more
        -- than we started with.
        Increment ->
            ( { model | count = model.count + 1 }, Cmd.none )

        -- The decrement message! Let's decrement.
        Decrement ->
            ( { model | count = model.count - 1 }, Cmd.none )


view : Model -> Html Message
view model =
    div []
        [ button [ onClick Increment ]
            [ text "+1" ]
        , div []
            [ text (String.fromInt model.count)
            ]
        , button [ onClick Decrement ] [ text "-1" ]
        ]


subscriptions : Model -> Sub message
subscriptions _ =
    Sub.none


decodeStory : Decode.Decoder Story
decodeStory =
    { scene = []
    }
        |> Decode.succeed
