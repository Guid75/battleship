port module Main exposing (main)

import Animator
import Animator.Inline
import Array
import Browser
import Dict exposing (Dict)
import Dict.Extra
import Element exposing (Element, alignRight, centerY, column, el, fill, padding, rgb255, row, spacing, text, width)
import GenLevel
import Grid exposing (drawGrid)
import Html exposing (Html, button, div, span, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Html.Events.Extra.Mouse as Mouse
import Matrix exposing (Matrix)
import Random
import Random.List
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)
import Time
import Types exposing (Boat, BoatDef, CellType(..), Direction(..), Grid, GridCoord, Model, Msg(..), Turn(..))


boatDefs : List BoatDef
boatDefs =
    [ BoatDef "Carrier" 5
    , BoatDef "Battleship" 4
    , BoatDef "Cruiser" 3
    , BoatDef "Submarine" 3
    , BoatDef "Destroyer" 2
    ]


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


createFreshMatrix : Matrix CellType
createFreshMatrix =
    Matrix.repeat 10 10 Free


initModel : Model -> Turn -> Model
initModel model turn =
    let
        newModel =
            { model | boatsToPlace = boatDefs }
    in
    case turn of
        Player ->
            { newModel | myBoard = { matrix = createFreshMatrix, boats = Dict.empty } }

        CPU ->
            { newModel | cpuBoard = { matrix = createFreshMatrix, boats = Dict.empty } }


init : () -> ( Model, Cmd Msg )
init flags =
    ( { myBoard = { matrix = createFreshMatrix, boats = Dict.empty }
      , cpuBoard = { matrix = createFreshMatrix, boats = Dict.empty }
      , boatsToPlace = boatDefs
      , myGrid = Grid 10 10 10 1 2 30 { x = 0, y = 0 } "#000000"
      , cpuGrid = Grid 10 10 10 1 2 30 { x = 0, y = 0 } "#000000"
      , turn = Nothing
      , currentMousePos = { x = 0, y = 0 }
      , clickedBoat = Nothing
      , clickedCell = Nothing
      }
    , Cmd.none
    )


displayRow : Matrix CellType -> Int -> Html msg
displayRow matrix rowIndex =
    let
        cellTypeToHtml cellType =
            case cellType of
                Occupied ->
                    span [] [ Html.text "O" ]

                NextTo ->
                    span [] [ Html.text "|" ]

                Free ->
                    span [] [ Html.text "." ]
    in
    case Matrix.getRow rowIndex matrix of
        Ok row ->
            div
                []
            <|
                Array.toList <|
                    Array.map (\cellType -> cellTypeToHtml cellType) row

        Err _ ->
            div
                []
                []


sizeToColor : Int -> String
sizeToColor size =
    case size of
        2 ->
            "#FF8888"

        3 ->
            "#88FF88"

        4 ->
            "#8888FF"

        5 ->
            "#888888"

        _ ->
            "#888888"


boatToSvg grid boat =
    let
        id =
            boat.id

        color =
            sizeToColor boat.size
    in
    case boat.dir of
        South ->
            Grid.drawRect grid id color 2 boat.pos { width = 1, height = boat.size }

        East ->
            Grid.drawRect grid id color 2 boat.pos { width = boat.size, height = 1 }

        North ->
            Grid.drawRect grid id color 2 { col = boat.pos.col, row = boat.pos.row - boat.size + 1 } { width = 1, height = boat.size }

        West ->
            Grid.drawRect grid id color 2 { col = boat.pos.col - boat.size + 1, row = boat.pos.row } { width = boat.size, height = 1 }


generateBoatsSvg grid boats =
    boats
        |> Dict.values
        |> List.map (boatToSvg grid)


viewBoard grid model svgBoats id_ =
    svg
        [ id id_
        , Svg.Attributes.width "400"
        , height "400"
        , viewBox "0 0 400 400"
        , Mouse.onMove (MouseMove id_)
        , Mouse.onDown (MouseDown id_)
        , Mouse.onUp (MouseUp id_)
        ]
    <|
        List.concat [ [ drawGrid grid [] ], svgBoats ]


viewBoards model =
    let
        svgBoats =
            generateBoatsSvg model.myGrid model.myBoard.boats

        svgBoats2 =
            generateBoatsSvg model.cpuGrid model.cpuBoard.boats
    in
    row
        [ padding 40, Element.spacing 40 ]
        [ Element.html <| viewBoard model.myGrid model svgBoats "myBoard"
        , Element.html <| viewBoard model.cpuGrid model svgBoats2 "cpuBoard"
        ]


view model =
    Element.layout
        []
    <|
        column
            []
            [ viewBoards model
            , Element.html <| button [ Html.Events.onClick <| Generate Player ] [ Html.text "Generate for myself" ]
            , Element.html <| button [ Html.Events.onClick <| Generate CPU ] [ Html.text "Generate for CPU" ]
            ]


iterPlacement : GridCoord -> Direction -> Model -> ( Model, Cmd Msg )
iterPlacement pos dir model =
    let
        board =
            case model.turn of
                Just Player ->
                    model.myBoard

                _ ->
                    model.cpuBoard
    in
    case model.boatsToPlace of
        head :: tail ->
            case GenLevel.tryToPlace (Boat pos head.size dir head.id) board.matrix of
                ( matrix, Just boat ) ->
                    let
                        newModel =
                            case model.turn of
                                Just Player ->
                                    { model
                                        | myBoard =
                                            { board
                                                | matrix = matrix
                                                , boats = Dict.insert head.id boat board.boats
                                            }
                                    }

                                _ ->
                                    { model
                                        | cpuBoard =
                                            { board
                                                | matrix = matrix
                                                , boats = Dict.insert head.id boat board.boats
                                            }
                                    }
                    in
                    ( { newModel
                        | boatsToPlace = tail
                      }
                    , GenLevel.buildShuffleCommand <|
                        GenLevel.computeAvailableCells matrix
                    )

                _ ->
                    ( model, GenLevel.buildShuffleCommand <| GenLevel.computeAvailableCells board.matrix )

        [] ->
            ( model, Cmd.none )


mouseMoveOnMyBoard : ( Float, Float ) -> Model -> Model
mouseMoveOnMyBoard ( x, y ) model =
    let
        currentCell =
            Grid.getClosestCell { x = x, y = y } model.myGrid

        board =
            model.myBoard
    in
    case ( model.clickedBoat, model.clickedCell ) of
        ( Just boat, Just clickedCell ) ->
            let
                newBoat =
                    { boat | pos = { col = boat.pos.col + currentCell.col - clickedCell.col, row = boat.pos.row + currentCell.row - clickedCell.row } }

                newBoards =
                    { board | boats = Dict.insert boat.id newBoat board.boats }
            in
            { model | myBoard = newBoards }

        _ ->
            model


mouseMoveOnCPUBoard : ( Float, Float ) -> Model -> Model
mouseMoveOnCPUBoard ( x, y ) model =
    model


mouseMove : ( String, Float, Float ) -> Model -> Model
mouseMove ( id, x, y ) model =
    let
        newModel =
            { model | currentMousePos = { x = x, y = y } }
    in
    case id of
        "myBoard" ->
            mouseMoveOnMyBoard ( x, y ) newModel

        _ ->
            mouseMoveOnCPUBoard ( x, y ) newModel


isCellBelongToBoat : GridCoord -> Boat -> Bool
isCellBelongToBoat cell boat =
    GenLevel.computeBoatCellPositions boat
        |> List.any (\boatCell -> boatCell == cell)


getBoatByCell : GridCoord -> Model -> Maybe Boat
getBoatByCell cell model =
    case Dict.Extra.find (\_ boat -> isCellBelongToBoat cell boat) model.myBoard.boats of
        Just ( id, boat ) ->
            Just boat

        _ ->
            Nothing


mouseDownMyBoard : Mouse.Event -> Model -> Model
mouseDownMyBoard event model =
    let
        cell =
            Grid.getClosestCell model.currentMousePos model.myGrid

        maybeBoat =
            getBoatByCell cell model
    in
    { model | clickedBoat = maybeBoat, clickedCell = Just cell }


mouseDownCpuBoard : Mouse.Event -> Model -> Model
mouseDownCpuBoard event model =
    model


mouseUpMyBoard : Mouse.Event -> Model -> Model
mouseUpMyBoard event model =
    model


mouseDown : String -> Mouse.Event -> Model -> Model
mouseDown boardId event model =
    case boardId of
        "myBoard" ->
            mouseDownMyBoard event model

        _ ->
            mouseDownCpuBoard event model


mouseUp : String -> Mouse.Event -> Model -> Model
mouseUp id event model =
    { model | clickedBoat = Nothing, clickedCell = Nothing }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    -- case msg of
    case Debug.log "msg" msg of
        GetCoordAndDirection ( pos, dir ) ->
            iterPlacement pos dir model

        Generate turn ->
            let
                newModel =
                    initModel model turn
            in
            ( { newModel | turn = Just turn }, GenLevel.buildShuffleCommand <| GenLevel.createBoatStartCouples 0 0 9 9 )

        PieceOver ->
            ( model, Cmd.none )

        PieceOut ->
            ( model, Cmd.none )

        MouseDown id event ->
            ( mouseDown id event model, Cmd.none )

        MouseUp id event ->
            ( mouseUp id event model, Cmd.none )

        MouseMove id event ->
            let
                ( x, y ) =
                    event.pagePos
            in
            ( model, requestSvgMousePos ( id, truncate x, truncate y ) )

        SvgMousePosResult pos ->
            ( mouseMove pos model, Cmd.none )


port requestSvgMousePos : ( String, Int, Int ) -> Cmd msg


port svgMousePosResult : (( String, Float, Float ) -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
    svgMousePosResult SvgMousePosResult
