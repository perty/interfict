module Editor exposing (Message(..), Model, init, initialModel, subscriptions, update, view)

-- To differentiate from SVG attributes, HA is used.

import Browser.Dom
import Browser.Events
import Dict exposing (Dict)
import Html exposing (Html, div, input)
import Html.Attributes as HA
import Html.Events
import Json.Decode as Decode
import Svg exposing (circle, path, svg)
import Svg.Attributes exposing (cx, cy, d, fill, fillOpacity, height, id, r, startOffset, stroke, strokeOpacity, strokeWidth, style, transform, viewBox, width, xlinkHref)
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


type alias Model =
    { scale : Float
    , graphElement : GraphElement
    , clicked : Bool
    , dragState : DragState
    , nodes : Dict NodeId Node
    , edges : List Edge
    }


type alias Node =
    { position : Position
    }


type alias NodeId =
    Int


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
    }


type DragState
    = Static
    | Moving NodeId


init : ( Model, Cmd Message )
init =
    ( initialModel, Browser.Dom.getElement graphId |> Task.attempt GotDomElement )


initialModel : Model
initialModel =
    { scale = 1.0
    , graphElement = { position = Position 0 0, dimension = Dimension 0 0 }
    , clicked = False
    , dragState = Static
    , nodes = Dict.fromList [ ( 1, node1 ), ( 2, node2 ) ]
    , edges = [ Edge 1 2 ]
    }


node1 : Node
node1 =
    { position = Position 50 50
    }


node2 : Node
node2 =
    { position = Position 75 75
    }


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
                newNode =
                    Node (fromScreen pos model.scale model.graphElement)
            in
            ( { model
                | nodes = Dict.insert nodeId newNode model.nodes
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
                newNode =
                    Node (fromScreen pos model.scale model.graphElement)
            in
            ( { model | dragState = Static, nodes = Dict.insert nodeId newNode model.nodes }
            , Cmd.none
            )

        SetZoom floatString ->
            case String.toFloat floatString of
                Nothing ->
                    ( model, Cmd.none )

                Just float ->
                    ( { model | scale = float }, Cmd.none )

        WindowResize ->
            ( model, Browser.Dom.getElement graphId |> Task.attempt GotDomElement )

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


getNode nodes nodeId =
    Dict.get nodeId nodes |> Maybe.withDefault (Node (Position 0 0))


viewPortWidth =
    100


viewPortHeight =
    100


fromScreen : Position -> Float -> GraphElement -> Position
fromScreen position zoom graphElement =
    Position ((position.x - graphElement.position.x) / graphElement.dimension.width * viewPortWidth / zoom)
        ((position.y - graphElement.position.y) / graphElement.dimension.height * viewPortHeight / zoom)


view : Model -> Html Message
view model =
    div [ HA.style "height" "100%" ]
        [ viewZoomControl model
        , viewGraph model
        ]


viewZoomControl : Model -> Html Message
viewZoomControl model =
    input
        [ HA.type_ "range"
        , HA.min "0.1"
        , HA.max "10"
        , HA.step "0.1"
        , HA.value (String.fromFloat model.scale)
        , Html.Events.onInput SetZoom
        , HA.style "width" (String.fromFloat model.graphElement.dimension.width ++ "px")
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
                        (drawEdges model.edges model.nodes
                            ++ drawNodes model.nodes
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


drawEdges : List Edge -> Dict NodeId Node -> List (Svg.Svg Message)
drawEdges edges dict =
    List.map (\e -> drawEdge e dict) edges


drawEdge : Edge -> Dict NodeId Node -> Svg.Svg Message
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
            "edge" ++ String.fromInt edge.fromNode ++ "-" ++ String.fromInt edge.toNode
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
                , style "font-size:6"
                ]
                [ Svg.text "hej" ]
            ]
        ]


drawNodes : Dict NodeId Node -> List (Svg.Svg Message)
drawNodes dict =
    Dict.toList dict |> List.map (\( id, node ) -> drawNode id node)


drawNode : NodeId -> Node -> Svg.Svg Message
drawNode nodeId node =
    circle
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


subscriptions : Model -> Sub Message
subscriptions model =
    case model.dragState of
        Static ->
            Browser.Events.onResize (\_ _ -> WindowResize)

        Moving id ->
            Sub.batch
                [ Browser.Events.onMouseMove (Decode.map2 (DragMove id) decodeButtons decodePosition)
                , Browser.Events.onMouseUp (Decode.map (DragStop id) decodePosition)
                ]


decodePosition : Decode.Decoder Position
decodePosition =
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
