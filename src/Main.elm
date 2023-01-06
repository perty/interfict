module Main exposing (decodeStory, main)

import Browser
import Dict exposing (Dict)
import Html exposing (Html, button, div, h1, p, text)
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
    { story : Story
    , texts : Dict Home StoryText
    , viewMode : ViewMode
    , currentScene : Maybe Scene
    }


type alias Story =
    { scene : List Scene
    }


type alias Scene =
    { home : Home
    , name : String
    , route : List SceneOption
    }


type alias SceneOption =
    { optionText : String
    , target : String
    }


type alias Home =
    String


type alias StoryText =
    String


type ViewMode
    = Library
    | ViewStory


init : flags -> ( Model, Cmd message )
init _ =
    ( { story = { scene = [] }
      , texts = Dict.empty
      , viewMode = Library
      , currentScene = Nothing
      }
    , Cmd.none
    )


type Message
    = LoadStory
    | StoryLoaded (Result Http.Error Story)
    | StoryTextLoaded Home (Result Http.Error StoryText)


update : Message -> Model -> ( Model, Cmd Message )
update message model =
    case message of
        LoadStory ->
            ( model, getStory StoryLoaded )

        StoryLoaded (Ok story) ->
            ( { model | story = story, viewMode = ViewStory, currentScene = List.head story.scene }, getStoryTexts story )

        StoryLoaded (Err _) ->
            ( model, Cmd.none )

        StoryTextLoaded home (Ok storyText) ->
            ( { model | texts = Dict.insert home storyText model.texts }, Cmd.none )

        StoryTextLoaded _ (Err _) ->
            ( model, Cmd.none )


view : Model -> Html Message
view model =
    case model.viewMode of
        Library ->
            viewLibrary

        ViewStory ->
            viewStory model


viewLibrary : Html.Html Message
viewLibrary =
    div []
        [ button [ onClick LoadStory ]
            [ text "Load" ]
        ]


viewStory : Model -> Html Message
viewStory model =
    let
        title =
            model.currentScene |> Maybe.map .name |> Maybe.withDefault "??"

        maybeHome =
            model.currentScene |> Maybe.map .home

        content =
            case maybeHome of
                Nothing ->
                    "??"

                Just home ->
                    Dict.get home model.texts |> Maybe.withDefault "??"
    in
    div []
        [ h1 [] [ text title ]
        , div [] (textToParagraphs content)
        ]


textToParagraphs : StoryText -> List (Html Message)
textToParagraphs storyText =
    storyText
        |> String.split "\n\n"
        |> List.map (\s -> p [] [ text s ])


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
        { url = "/story/scenes.json"
        , expect = Http.expectJson message decodeStory
        }


getStoryTexts : Story -> Cmd Message
getStoryTexts story =
    story.scene
        |> List.map .home
        |> List.map getStoryText
        |> Cmd.batch


getStoryText : Home -> Cmd Message
getStoryText home =
    Http.get
        { url = "/story/text/" ++ home ++ ".txt"
        , expect = Http.expectString (StoryTextLoaded home)
        }
