module Main exposing (decodeStory, main)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (required)


main : Program () Model Message
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type alias Model =
    { story : Story }


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
    ( { story = { scene = [] } }, Cmd.none )


type Message
    = LoadStory
    | StoryLoaded (Result Http.Error Story)


update : Message -> Model -> ( Model, Cmd Message )
update message model =
    case message of
        LoadStory ->
            ( model, getStory StoryLoaded )

        StoryLoaded (Ok story) ->
            ( { model | story = story }, Cmd.none )

        StoryLoaded (Err _) ->
            ( model, Cmd.none )


view : Model -> Html Message
view model =
    div []
        [ button [ onClick LoadStory ]
            [ text "Load" ]
        ]


subscriptions : Model -> Sub message
subscriptions _ =
    Sub.none


decodeStory : Decode.Decoder Story
decodeStory =
    Decode.succeed Story
        |> required "scene" (Decode.list decodeScene)


decodeScene : Decode.Decoder Scene
decodeScene =
    Decode.succeed Scene
        |> required "home" Decode.string
        |> required "name" Decode.string
        |> required "route" (Decode.list decodeSceneOption)


decodeSceneOption : Decode.Decoder SceneOption
decodeSceneOption =
    Decode.succeed SceneOption
        |> required "optionText" Decode.string
        |> required "target" Decode.string


getStory : (Result Http.Error Story -> Message) -> Cmd Message
getStory message =
    Http.get
        { url = "/story/story.json"
        , expect = Http.expectJson message decodeStory
        }
