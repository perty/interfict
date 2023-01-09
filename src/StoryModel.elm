module StoryModel exposing
    ( Home
    , Model
    , OptionText
    , Scene
    , SceneOption
    , Story
    , StoryImage
    , StoryLocation
    , StoryText
    , getScene
    , getStory
    , getText
    , hasImage
    , init
    , setImage
    , setStory
    , setText
    )

{-
   The StoryModel is common to both the Reader and Editor.
-}

import Dict exposing (Dict)
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (required)


type alias Model =
    { story : Story
    , texts : Dict Home StoryText
    , images : Dict Home StoryImage
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
    { optionText : OptionText
    , target : Home
    }


type alias Home =
    String


type alias StoryText =
    String


type alias StoryImage =
    String


type alias StoryLocation =
    String


type alias OptionText =
    String


init : Model
init =
    { story = { scene = [] }
    , texts = Dict.empty
    , images = Dict.empty
    }


getStory : String -> (Result Http.Error Story -> message) -> Cmd message
getStory storyLocation message =
    Http.get
        { url = storyLocation ++ "/scenes.json"
        , expect = Http.expectJson message decodeStory
        }


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


setStory : Model -> Story -> Model
setStory model story =
    { model | story = story }


setText : Model -> Home -> StoryText -> Model
setText model home storyText =
    { model | texts = Dict.insert home storyText model.texts }


getText : Model -> Home -> StoryText
getText model home =
    Dict.get home model.texts |> Maybe.withDefault "??"


setImage : Model -> Home -> Model
setImage model home =
    { model | images = Dict.insert home home model.images }


getScene : Model -> Home -> Maybe Scene
getScene model home =
    model.story.scene |> List.filter (\scene -> scene.home == home) |> List.head


hasImage : Model -> Home -> Bool
hasImage model home =
    case Dict.get home model.images of
        Just _ ->
            True

        Nothing ->
            False
