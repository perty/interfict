port module Editor exposing (Message(..), Model, graphLoaded, init, initialModel, loadGraph, setStory, storeGraph, subscriptions, update, view)

{-
   The editor lets you see the story as a network of connected nodes.

   The graphical presentation is a separate structure, it has other concerns and it might be
   necessary to have a story node on more than one place in the graphical view, for practical
   purposes.
-}

import Browser.Dom
import Browser.Events
import Dict exposing (Dict)
import File.Download
import Html exposing (Html, button, div, input)
import Html.Attributes as HA
import Html.Events
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode
import StoryModel exposing (Home, OptionText, Scene, Story)
import Svg exposing (circle, path, svg)
import Svg.Attributes exposing (cx, cy, d, fill, fillOpacity, height, id, r, startOffset, stroke, strokeOpacity, strokeWidth, style, transform, viewBox, width, x, xlinkHref, y)
import Svg.Events exposing (onClick, onMouseDown)
import Task


type Message
    = ClickedCircle
    | DragStart NodeId
    | DragMove NodeId Bool Position
    | DragStop NodeId Position
    | SetZoom String
    | GotDomElement (Result Browser.Dom.Error Browser.Dom.Element)
    | WindowResize
    | GraphLoadedFromLocalStorage String
    | DumpGraph


type alias Model =
    { scale : Float
    , graphElement : GraphElement
    , clicked : Bool
    , dragState : DragState
    , graph : NodeGraph
    , decodeError : Maybe Decode.Error
    }


type alias NodeGraph =
    { nodes : Nodes
    , edges : List Edge
    }


type alias Nodes =
    Dict NodeId Node


type alias Node =
    { position : Position
    , scene : Scene
    }


type alias NodeId =
    Home


type alias GraphElement =
    { position : Position
    , dimension : Dimension
    }


type alias Position =
    { x : Float
    , y : Float
    }


type alias Dimension =
    { width : Float
    , height : Float
    }


type alias Edge =
    { fromNode : NodeId
    , toNode : NodeId
    , label : OptionText
    }


type DragState
    = Static
    | Moving NodeId


port storeGraph : String -> Cmd msg


port loadGraph : () -> Cmd msg


port graphLoaded : (String -> msg) -> Sub msg


init : ( Model, Cmd Message )
init =
    ( initialModel, getDrawingArea )


initialModel : Model
initialModel =
    { scale = 1.0
    , graphElement = { position = Position 0 0, dimension = Dimension 0 0 }
    , clicked = False
    , dragState = Static
    , graph =
        { nodes = Dict.empty
        , edges = []
        }
    , decodeError = Nothing
    }


getDrawingArea : Cmd Message
getDrawingArea =
    Browser.Dom.getElement graphId |> Task.attempt GotDomElement


setStory : Model -> Story -> Model
setStory model story =
    let
        mkPos : Int -> Position
        mkPos n =
            Position (10.0 * Basics.toFloat n) (10.0 * Basics.toFloat n)

        newGraph =
            story.scene
                |> List.indexedMap (\n s -> ( s.home, { position = mkPos n, scene = s } ))
                |> Dict.fromList

        newEdges =
            story.scene
                |> List.map
                    (\s ->
                        s.route
                            |> List.map (\r -> Edge s.home r.target r.optionText)
                            |> List.filter (\e -> String.length e.label > 0)
                            |> mergeTargets
                    )
                |> List.concat
    in
    { model | graph = { nodes = newGraph, edges = newEdges } }


mergeTargets : List Edge -> List Edge
mergeTargets edges =
    case edges of
        [] ->
            edges

        head :: tail ->
            let
                newList =
                    mergeTarget head tail
            in
            List.append newList (mergeTargets tail)


mergeTarget : Edge -> List Edge -> List Edge
mergeTarget edge edges =
    case edges of
        [] ->
            [ edge ]

        head :: tail ->
            if head.toNode == edge.toNode then
                { head | label = head.label ++ "/" ++ edge.label } :: tail

            else
                mergeTarget edge tail


update : Message -> Model -> ( Model, Cmd Message )
update msg model =
    case msg of
        ClickedCircle ->
            ( { model | clicked = not model.clicked }, Cmd.none )

        DragStart nodeId ->
            ( { model | dragState = Moving nodeId }
            , Cmd.none
            )

        DragMove nodeId isDown pos ->
            let
                newPos =
                    fromScreen pos model.scale model.graphElement

                currentGraph =
                    model.graph
            in
            ( { model
                | graph = { currentGraph | nodes = updateNodePos nodeId newPos model.graph.nodes }
                , dragState =
                    if isDown then
                        Moving nodeId

                    else
                        Static
              }
            , Cmd.none
            )

        DragStop nodeId pos ->
            let
                newPos =
                    fromScreen pos model.scale model.graphElement

                currentGraph =
                    model.graph
            in
            ( { model
                | graph = { currentGraph | nodes = updateNodePos nodeId newPos model.graph.nodes }
                , dragState = Static
              }
            , storeGraph (encodeGraph model.graph)
            )

        GraphLoadedFromLocalStorage json ->
            case Decode.decodeString decodeGraph json of
                Ok graph ->
                    ( { model | graph = graph }, getDrawingArea )

                Err error ->
                    ( { model | decodeError = Just error }, Cmd.none )

        DumpGraph ->
            ( model, File.Download.string "graph.json" "application/json" (encodeGraph model.graph) )

        SetZoom floatString ->
            case String.toFloat floatString of
                Nothing ->
                    ( model, Cmd.none )

                Just float ->
                    ( { model | scale = float }, Cmd.none )

        WindowResize ->
            ( model, getDrawingArea )

        GotDomElement result ->
            case result of
                Err _ ->
                    ( model, Cmd.none )

                Ok domElement ->
                    let
                        side =
                            Basics.min domElement.element.height domElement.element.width - 2
                    in
                    ( { model
                        | graphElement =
                            { position =
                                { x = domElement.element.x
                                , y = domElement.element.y
                                }
                            , dimension =
                                { width = side
                                , height = side
                                }
                            }
                      }
                    , Cmd.none
                    )


getNode : Nodes -> NodeId -> Node
getNode nodes nodeId =
    Dict.get nodeId nodes |> Maybe.withDefault (Node (Position 0 0) emptyScene)


emptyScene : Scene
emptyScene =
    { home = ""
    , name = ""
    , route = []
    }


updateNodePos : NodeId -> Position -> Nodes -> Nodes
updateNodePos nodeId position nodeGraph =
    let
        currentNode =
            getNode nodeGraph nodeId
    in
    Dict.insert nodeId { currentNode | position = position } nodeGraph


viewPortWidth : number
viewPortWidth =
    100


viewPortHeight : number
viewPortHeight =
    100


fromScreen : Position -> Float -> GraphElement -> Position
fromScreen position zoom graphElement =
    Position ((position.x - graphElement.position.x) / graphElement.dimension.width * viewPortWidth / zoom)
        ((position.y - graphElement.position.y) / graphElement.dimension.height * viewPortHeight / zoom)


view : Model -> Html Message
view model =
    div [ HA.style "height" "100%" ]
        [ div [ HA.style "width" (String.fromFloat model.graphElement.dimension.width ++ "px") ]
            [ viewCommandBar
            , viewZoomControl model.scale
            ]
        , viewGraph model
        ]


viewCommandBar : Html Message
viewCommandBar =
    div [] [ button [ onClick DumpGraph ] [ Html.text "Dump" ] ]


viewZoomControl : Float -> Html Message
viewZoomControl scale_ =
    input
        [ HA.type_ "range"
        , HA.min "0.1"
        , HA.max "10"
        , HA.step "0.1"
        , HA.value (String.fromFloat scale_)
        , Html.Events.onInput SetZoom
        , HA.style "width" "100%"
        ]
        []


graphId =
    "graph"


viewGraph : Model -> Html Message
viewGraph model =
    Html.div [ HA.id graphId, HA.style "width" "100%", HA.style "height" "100%" ]
        [ svg
            [ width <| String.fromFloat model.graphElement.dimension.width
            , height <| String.fromFloat model.graphElement.dimension.height
            , viewBox <| "0 0 " ++ String.fromInt viewPortWidth ++ " " ++ String.fromInt viewPortHeight
            ]
            (viewBoxBackground
                :: [ Svg.g [ transform (scale model.scale) ]
                        (drawEdges model.graph
                            ++ drawNodes model.graph.nodes
                        )
                   ]
            )
        ]


scale : Float -> String
scale zoom =
    "scale(" ++ String.fromFloat zoom ++ ", " ++ String.fromFloat zoom ++ ")"


viewBoxBackground : Svg.Svg Message
viewBoxBackground =
    Svg.g []
        [ Svg.rect
            [ stroke "black"
            , strokeWidth "0.1"
            , width "100"
            , height "100"
            , fillOpacity "0.1"
            ]
            []
        ]


drawEdges : NodeGraph -> List (Svg.Svg Message)
drawEdges graph =
    List.map (\e -> drawEdge e graph.nodes) graph.edges


drawEdge : Edge -> Nodes -> Svg.Svg Message
drawEdge edge nodes =
    let
        fromNode =
            getNode nodes edge.fromNode

        toNode =
            getNode nodes edge.toNode

        pathPoints fn tn =
            "M"
                ++ String.fromFloat fn.position.x
                ++ ","
                ++ String.fromFloat fn.position.y
                ++ " L"
                ++ String.fromFloat tn.position.x
                ++ ","
                ++ String.fromFloat tn.position.y

        idString =
            "edge" ++ edge.fromNode ++ "-" ++ edge.toNode
    in
    Svg.g []
        [ path
            [ id idString
            , d <|
                if fromNode.position.x > toNode.position.x then
                    pathPoints toNode fromNode

                else
                    pathPoints fromNode toNode
            , stroke "black"
            , strokeWidth "0.1"
            ]
            []
        , Svg.text_ []
            [ Svg.textPath
                [ xlinkHref ("#" ++ idString)
                , startOffset "20%"
                , style "font-size:2"
                ]
                [ Svg.text edge.label ]
            ]
        ]


drawNodes : Dict NodeId Node -> List (Svg.Svg Message)
drawNodes dict =
    Dict.toList dict |> List.map (\( id, node ) -> drawNode id node)


drawNode : NodeId -> Node -> Svg.Svg Message
drawNode nodeId node =
    Svg.g []
        [ circle
            [ cx <| String.fromFloat node.position.x
            , cy <| String.fromFloat node.position.y
            , r "5"
            , stroke "black"
            , strokeWidth "0.4"
            , strokeOpacity "0.5"
            , fill "rgb(216,196,30)"
            , fillOpacity "1"
            , onClick ClickedCircle
            , onMouseDown (DragStart nodeId)
            ]
            []
        , Svg.text_
            [ x <| String.fromFloat (node.position.x - 4)
            , y <| String.fromFloat (node.position.y + 1)
            , style "font-size:2"
            ]
            [ Svg.text nodeId
            ]
        ]


subscriptions : Model -> Sub Message
subscriptions model =
    case model.dragState of
        Static ->
            Sub.batch
                [ Browser.Events.onResize (\_ _ -> WindowResize)
                , graphLoaded GraphLoadedFromLocalStorage
                ]

        Moving id ->
            Sub.batch
                [ Browser.Events.onMouseMove (Decode.map2 (DragMove id) decodeButtons decodeMousePosition)
                , Browser.Events.onMouseUp (Decode.map (DragStop id) decodeMousePosition)
                ]


decodeMousePosition : Decode.Decoder Position
decodeMousePosition =
    Decode.map2 Position decodeFractionX decodeFractionY


decodeFractionX : Decode.Decoder Float
decodeFractionX =
    Decode.field "pageX" Decode.float


decodeFractionY : Decode.Decoder Float
decodeFractionY =
    Decode.field "pageY" Decode.float


decodeButtons : Decode.Decoder Bool
decodeButtons =
    Decode.field "buttons" (Decode.map (\buttons -> buttons == 1) Decode.int)


encodeGraph : NodeGraph -> String
encodeGraph graph =
    Encode.object
        [ ( "nodes", Encode.dict identity encodeNode graph.nodes )
        , ( "edges", Encode.list encodeEdge graph.edges )
        ]
        |> Encode.encode 0


decodeGraph : Decode.Decoder NodeGraph
decodeGraph =
    Decode.succeed NodeGraph
        |> required "nodes" (Decode.dict decodeNode)
        |> required "edges" (Decode.list decodeEdge)


encodeNode : Node -> Encode.Value
encodeNode node =
    Encode.object
        [ ( "position", encodePosition node.position )
        , ( "scene", StoryModel.encodeScene node.scene )
        ]


decodeNode : Decode.Decoder Node
decodeNode =
    Decode.succeed Node
        |> required "position" decodePosition
        |> required "scene" StoryModel.decodeScene


encodePosition : Position -> Encode.Value
encodePosition position =
    Encode.object
        [ ( "x", Encode.float position.x )
        , ( "y", Encode.float position.y )
        ]


decodePosition : Decode.Decoder Position
decodePosition =
    Decode.succeed Position
        |> required "x" Decode.float
        |> required "y" Decode.float


encodeEdge : Edge -> Encode.Value
encodeEdge edge =
    Encode.object
        [ ( "fromNode", Encode.string edge.fromNode )
        , ( "toNode", Encode.string edge.toNode )
        , ( "label", Encode.string edge.label )
        ]


decodeEdge : Decode.Decoder Edge
decodeEdge =
    Decode.succeed Edge
        |> required "fromNode" Decode.string
        |> required "toNode" Decode.string
        |> required "label" Decode.string
