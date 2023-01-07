module Main exposing (decodeStory, main)

import Browser exposing (Document)
import Browser.Dom as Dom
import Dict exposing (Dict)
import Html exposing (Html, button, div, h1, img, input, p, text)
import Html.Attributes exposing (size, src, style, value)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (required)
import Task


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
    , images : Dict Home StoryImage
    , viewMode : ViewMode
    , currentStoryLocation : StoryLocation
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


type ViewMode
    = Library
    | ViewStory


init : flags -> ( Model, Cmd message )
init _ =
    ( { story = { scene = [] }
      , texts = Dict.empty
      , images = Dict.empty
      , viewMode = Library
      , currentStoryLocation = "https://artcomputer.se/interfict/story"
      , currentScene = Nothing
      }
    , Cmd.none
    )


type Message
    = ChangeStoryLocation String
    | LoadStory
    | StoryLoaded (Result Http.Error Story)
    | StoryTextLoaded Home (Result Http.Error StoryText)
    | StoryImageFound Home (Result Http.Error StoryImage)
    | GotoScene Home
    | NoOp


update : Message -> Model -> ( Model, Cmd Message )
update message model =
    case message of
        ChangeStoryLocation storyLocation ->
            ( { model | currentStoryLocation = storyLocation }, Cmd.none )

        LoadStory ->
            ( model, getStory model.currentStoryLocation StoryLoaded )

        StoryLoaded (Ok story) ->
            ( { model | story = story, viewMode = ViewStory, currentScene = List.head story.scene }
            , Cmd.batch
                [ getStoryTexts model.currentStoryLocation story
                , getStoryImages model.currentStoryLocation story
                ]
            )

        StoryLoaded (Err _) ->
            ( model, Cmd.none )

        StoryTextLoaded home (Ok storyText) ->
            ( { model | texts = Dict.insert home storyText model.texts }, Cmd.none )

        StoryTextLoaded _ (Err _) ->
            ( model, Cmd.none )

        StoryImageFound home (Ok _) ->
            ( { model | images = Dict.insert home home model.images }, Cmd.none )

        StoryImageFound _ (Err _) ->
            ( model, Cmd.none )

        GotoScene home ->
            ( { model | currentScene = model.story.scene |> List.filter (\scene -> scene.home == home) |> List.head }
            , resetViewport
            )

        NoOp ->
            ( model, Cmd.none )


resetViewport : Cmd Message
resetViewport =
    Task.perform (\_ -> NoOp) (Dom.setViewport 0 0)


view : Model -> Html Message
view model =
    case model.viewMode of
        Library ->
            viewLibrary model

        ViewStory ->
            viewStory model


viewLibrary : Model -> Html.Html Message
viewLibrary model =
    div
        [ style "display" "flex"
        , style "flex-direction" "column"
        , style "justify-content" "flex-start"
        , style "max-width" "600px"
        , style "height" "100%"
        , style "margin" "auto"
        , style "background-image" "url(assets/images/sky.png)"
        , style "background-size" "cover"
        , style "color" "lightgrey"
        ]
        [ h1 [ style "text-align" "center" ] [ text "Welcome to the library" ]
        , div
            [ style "display" "flex"
            , style "flex-wrap" "wrap"
            , style "width" "100%"
            , style "justify-content" "space-evenly"
            ]
            [ text "Load a story"
            , input [ value model.currentStoryLocation, onInput ChangeStoryLocation, size 35 ] []
            , button [ onClick LoadStory ]
                [ text "Load" ]
            ]
        ]


viewStory : Model -> Html Message
viewStory model =
    let
        title =
            model.currentScene |> Maybe.map .name |> Maybe.withDefault "??"

        home =
            model.currentScene |> Maybe.map .home |> Maybe.withDefault "??"

        content =
            Dict.get home model.texts |> Maybe.withDefault "??"

        route =
            model.currentScene |> Maybe.map .route |> Maybe.withDefault []

        hasImage =
            case Dict.get home model.images of
                Just _ ->
                    True

                Nothing ->
                    False
    in
    div
        [ style "max-width" "600px"
        , style "margin" "auto"
        ]
        [ if hasImage then
            img [ src ("story/images/" ++ home ++ ".png"), style "width" "100%" ] []

          else
            div [] []
        , h1 [ style "text-align" "center" ] [ text title ]
        , div [ style "margin" "5px" ] (textToParagraphs content)
        , viewOptions route
        ]


viewOptions : List SceneOption -> Html Message
viewOptions sceneOptions =
    div
        [ style "display" "flex"
        , style "width" "100%"
        , style "justify-content" "space-evenly"
        , style "padding-bottom" "50px"
        ]
        (List.map viewOption sceneOptions)


viewOption : SceneOption -> Html Message
viewOption sceneOption =
    button [ onClick (GotoScene sceneOption.target) ] [ text sceneOption.optionText ]


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


getStory : String -> (Result Http.Error Story -> Message) -> Cmd Message
getStory storyLocation message =
    Http.get
        { url = storyLocation ++ "/scenes.json"
        , expect = Http.expectJson message decodeStory
        }


getStoryTexts : StoryLocation -> Story -> Cmd Message
getStoryTexts storyLocation story =
    story.scene
        |> List.map .home
        |> List.map (getStoryText storyLocation)
        |> Cmd.batch


getStoryText : StoryLocation -> Home -> Cmd Message
getStoryText storyLocation home =
    Http.get
        { url = storyLocation ++ "/text/" ++ home ++ ".txt"
        , expect = Http.expectString (StoryTextLoaded home)
        }


getStoryImages : StoryLocation -> Story -> Cmd Message
getStoryImages storyLocation story =
    story.scene
        |> List.map .home
        |> List.map (getStoryImage storyLocation)
        |> Cmd.batch


getStoryImage : StoryLocation -> Home -> Cmd Message
getStoryImage storyLocation home =
    Http.get
        { url = storyLocation ++ "/images/" ++ home ++ ".png"
        , expect = Http.expectString (StoryImageFound home)
        }
