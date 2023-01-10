module Main exposing (main)

import Browser exposing (Document)
import Browser.Dom as Dom
import Editor
import Html exposing (Html, button, div, h1, h3, img, input, p, text)
import Html.Attributes exposing (size, src, style, value)
import Html.Events exposing (onClick, onInput)
import Http
import StoryModel exposing (Home, Scene, SceneOption, Story, StoryImage, StoryLocation, StoryText)
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
    { storyModel : StoryModel.Model
    , viewMode : ViewMode
    , currentStoryLocation : StoryLocation
    , currentScene : Maybe Scene
    , showHelp : Bool
    , editorModel : Editor.Model
    }


type ViewMode
    = Library
    | ViewStory
    | ViewEditor


init : flags -> ( Model, Cmd Message )
init _ =
    let
        ( editorModel, _ ) =
            Editor.init
    in
    ( { storyModel = StoryModel.init
      , viewMode = Library
      , currentStoryLocation = "https://artcomputer.se/interfict/story"
      , currentScene = Nothing
      , showHelp = False
      , editorModel = editorModel
      }
    , Cmd.none
    )


type Message
    = ChangeStoryLocation String
    | LoadStory
    | LoadEditor
    | ReloadEditorSession
    | StoryLoaded (Result Http.Error Story)
    | StoryTextLoaded Home (Result Http.Error StoryText)
    | StoryImageFound Home (Result Http.Error StoryImage)
    | GotoScene Home
    | NoOp
    | EditorMessage Editor.Message
    | ToggleHelp


update : Message -> Model -> ( Model, Cmd Message )
update message model =
    case message of
        EditorMessage editorMessage ->
            updateEditor editorMessage model

        ChangeStoryLocation storyLocation ->
            ( { model | currentStoryLocation = storyLocation }, Cmd.none )

        LoadStory ->
            ( { model | viewMode = ViewStory }, StoryModel.getStory model.currentStoryLocation StoryLoaded )

        LoadEditor ->
            ( { model | viewMode = ViewEditor }, StoryModel.getStory model.currentStoryLocation StoryLoaded )

        ReloadEditorSession ->
            ( { model | viewMode = ViewEditor }, Editor.loadGraph () )

        StoryLoaded (Ok story) ->
            let
                ( _, editCmd ) =
                    Editor.init
            in
            ( { model
                | storyModel = StoryModel.setStory model.storyModel story
                , editorModel = Editor.setStory model.editorModel story
                , currentScene = List.head story.scene
              }
            , Cmd.batch
                [ getStoryTexts model.currentStoryLocation story
                , getStoryImages model.currentStoryLocation story
                , Cmd.map EditorMessage editCmd
                ]
            )

        StoryLoaded (Err _) ->
            ( model, Cmd.none )

        StoryTextLoaded home (Ok storyText) ->
            ( { model | storyModel = StoryModel.setText model.storyModel home storyText }, Cmd.none )

        StoryTextLoaded _ (Err _) ->
            ( model, Cmd.none )

        StoryImageFound home (Ok _) ->
            ( { model | storyModel = StoryModel.setImage model.storyModel home }, Cmd.none )

        StoryImageFound _ (Err _) ->
            ( model, Cmd.none )

        GotoScene home ->
            ( { model | currentScene = StoryModel.getScene model.storyModel home }
            , resetViewport
            )

        NoOp ->
            ( model, Cmd.none )

        ToggleHelp ->
            ( { model | showHelp = not model.showHelp }, Cmd.none )


resetViewport : Cmd Message
resetViewport =
    Task.perform (\_ -> NoOp) (Dom.setViewport 0 0)


updateEditor : Editor.Message -> Model -> ( Model, Cmd Message )
updateEditor message model =
    let
        ( editorModel, editorCommand ) =
            Editor.update message model.editorModel
    in
    ( { model | editorModel = editorModel }
    , Cmd.map EditorMessage editorCommand
    )


view : Model -> Html Message
view model =
    case model.viewMode of
        Library ->
            viewLibrary model

        ViewStory ->
            viewStory model

        ViewEditor ->
            Editor.view model.editorModel |> Html.map EditorMessage


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
            , button [ onClick LoadStory ] [ text "Load" ]
            , button [ onClick LoadEditor ] [ text "Edit" ]
            ]
        , div
            [ style "display" "flex"
            , style "justify-content" "flex-end"
            , style "padding-top" "50px"
            , style "padding-right" "50px"
            ]
            [ button [ onClick ReloadEditorSession ] [ text "Reload editing session" ] ]
        , if model.showHelp then
            viewHelp

          else
            div
                [ style "position" "relative"
                , style "left" "250px"
                ]
                [ button [ onClick ToggleHelp, style "font-size" "x-large" ] [ text "?" ]
                ]
        ]


viewHelp : Html Message
viewHelp =
    div
        [ style "background-color" "white"
        , style "max-width" "75%"
        , style "color" "darkgrey"
        , style "margin" "auto"
        , style "border-radius" "10px"
        , style "padding" "10px"
        ]
        ([ h3 [] [ text "Help" ] ]
            ++ textToParagraphs helpText
            ++ [ button [ onClick ToggleHelp ] [ text "Close" ] ]
        )


helpText : String
helpText =
    """
    This is a reader and editor for interactive fiction, also called
    create your own adventure. The reader is started by loading the story
    from an URL. An example is provided.

    The editor allows you to see a graphical presentation of the possible
    paths through the story. If you never edited the story, the initial
    position of the nodes is set along a line from top left to bottom right.

    As soon as you change the position of a node by dragging it, the position
    is stored in your browser. To continue from that, use reload editing session
    button on the first page.

    If you like to store your editing session on file, there is a button
    inside the editor to download it as a json file.
    Conversely, you may upload a session from a file.

    The slider on the top of the editor is a zoom control.

    You can pan the view by dragging the background.

    The editor is resizing itself with the window but stays square.
    """


viewStory : Model -> Html Message
viewStory model =
    let
        title =
            model.currentScene |> Maybe.map .name |> Maybe.withDefault "??"

        home =
            model.currentScene |> Maybe.map .home |> Maybe.withDefault "??"

        route =
            model.currentScene |> Maybe.map .route |> Maybe.withDefault []
    in
    div
        [ style "max-width" "600px"
        , style "margin" "auto"
        ]
        [ if StoryModel.hasImage model.storyModel home then
            img [ src (model.currentStoryLocation ++ "/images/" ++ home ++ ".png"), style "width" "100%" ] []

          else
            div [] []
        , h1 [ style "text-align" "center" ] [ text title ]
        , div [ style "margin" "5px" ] (StoryModel.getText model.storyModel home |> textToParagraphs)
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


subscriptions : Model -> Sub Message
subscriptions model =
    Editor.subscriptions model.editorModel |> Sub.map EditorMessage


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
