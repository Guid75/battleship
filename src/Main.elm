port module Main exposing (main)

import Animator
import Animator.Inline
import Array
import Browser
import Color
import Dict exposing (Dict)
import Dict.Extra
import Element exposing (Element, alignRight, centerY, column, el, fill, padding, rgb255, row, spacing, text, width)
import Figures
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


animator : Animator.Animator Model
animator =
    Animator.animator
        |> Animator.watching
            .focusedUp
            (\newFocusedUp model ->
                { model | focusedUp = newFocusedUp }
            )
        |> Animator.watching
            .fire
            (\newFire model ->
                { model | fire = newFire }
            )


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
        board =
            case turn of
                Player ->
                    model.myBoard

                CPU ->
                    model.cpuBoard

        newBoard =
            { board
                | matrix = createFreshMatrix
                , boatsToPlace = boatDefs
                , boats = Dict.empty
                , shots = []
                , cellOver = Nothing
            }
    in
    case turn of
        Player ->
            { model | myBoard = newBoard }

        CPU ->
            { model | cpuBoard = newBoard }


init : () -> ( Model, Cmd Msg )
init flags =
    ( { myBoard =
            { matrix = createFreshMatrix
            , boatsToPlace = boatDefs
            , boats = Dict.empty
            , shots = []
            , cellOver = Nothing
            , grid = Grid 10 10 10 1 2 30 { x = 0, y = 0 } "#000000"
            , id = "myBoard"
            }
      , cpuBoard =
            { matrix = createFreshMatrix
            , boatsToPlace = boatDefs
            , boats = Dict.empty
            , shots = []
            , cellOver = Nothing
            , grid = Grid 10 10 10 1 2 30 { x = 0, y = 0 } "#000000"
            , id = "cpuBoard"
            }
      , currentMousePos = { x = 0, y = 0 }
      , clickedBoat = Nothing
      , clickedCell = Nothing
      , focusedBoat = Nothing
      , focusedUp = Animator.init False
      , fire = Animator.init False
      }
    , Cmd.batch
        [ GenLevel.randomizeBoatPlacements Player <| GenLevel.createBoatStartCouples 0 0 9 9
        , GenLevel.randomizeBoatPlacements CPU <| GenLevel.createBoatStartCouples 0 0 9 9
        ]
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


sizeToColor : Int -> Color.Color
sizeToColor size =
    case size of
        2 ->
            Color.rgb255 255 136 136

        3 ->
            Color.rgb255 136 255 136

        4 ->
            Color.rgb255 136 136 255

        5 ->
            Color.rgb255 136 136 136

        _ ->
            Color.rgb255 136 136 136


boatToSvg grid boat focusedBoat model =
    let
        id =
            boat.id

        color =
            sizeToColor boat.size

        focused =
            case focusedBoat of
                Just boat_ ->
                    boat_.id == id

                _ ->
                    False

        c =
            Color.toRgba color

        computeColor f =
            let
                red =
                    c.red + (1.0 - c.red) * f

                green =
                    c.green + (1.0 - c.green) * f

                blue =
                    c.blue + (1.0 - c.blue) * f
            in
            Color.fromRgba { red = red, green = green, blue = blue, alpha = c.alpha }

        attrs =
            if focused then
                [ Animator.Inline.style model.focusedUp
                    "fill"
                    (\f -> Color.toCssString <| computeColor f)
                    (\state ->
                        if state then
                            Animator.at 0

                        else
                            Animator.at 0.6
                    )
                ]

            else
                []
    in
    case boat.dir of
        South ->
            Figures.drawRect attrs grid id color 2 boat.pos { width = 1, height = boat.size }

        East ->
            Figures.drawRect attrs grid id color 2 boat.pos { width = boat.size, height = 1 }

        North ->
            Figures.drawRect attrs grid id color 2 { col = boat.pos.col, row = boat.pos.row - boat.size + 1 } { width = 1, height = boat.size }

        West ->
            Figures.drawRect attrs grid id color 2 { col = boat.pos.col - boat.size + 1, row = boat.pos.row } { width = boat.size, height = 1 }


generateBoatsSvg grid boats focusedBoat model =
    boats
        |> Dict.values
        |> List.map (\boat -> boatToSvg grid boat focusedBoat model)


viewMyBoard model =
    let
        board =
            model.myBoard

        grid =
            board.grid

        svgBoats =
            generateBoatsSvg grid board.boats model.focusedBoat model
    in
    svg
        [ id board.id
        , Svg.Attributes.width "400"
        , height "400"
        , viewBox "0 0 400 400"
        , Mouse.onMove (MouseMove board.id)
        , Mouse.onDown (MouseDown board.id)
        , Mouse.onUp (MouseUp board.id)
        ]
    <|
        List.concat [ [ drawGrid grid [] ], svgBoats ]


viewCpuBoard model =
    let
        board =
            model.cpuBoard

        grid =
            board.grid

        a =
            Animator.linear model.fire <|
                \state ->
                    if state then
                        Animator.at 1

                    else
                        Animator.at 0

        cellOverSvg =
            case board.cellOver of
                Just coord ->
                    [ Figures.drawTarget
                        grid
                        Color.gray
                        coord
                        a
                    ]

                _ ->
                    []
    in
    svg
        [ id board.id
        , Svg.Attributes.width "400"
        , height "400"
        , viewBox "0 0 400 400"
        , Mouse.onMove (MouseMove board.id)
        , Mouse.onDown (MouseDown board.id)
        , Mouse.onUp (MouseUp board.id)
        ]
    <|
        List.concat [ [ drawGrid grid [] ], cellOverSvg ]


viewBoards model =
    row
        [ padding 40, Element.spacing 40 ]
        [ Element.html <| viewMyBoard model
        , Element.html <| viewCpuBoard model
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


iterPlacement : Turn -> GridCoord -> Direction -> Model -> ( Model, Cmd Msg )
iterPlacement turn pos dir model =
    let
        board =
            case turn of
                Player ->
                    model.myBoard

                CPU ->
                    model.cpuBoard
    in
    case board.boatsToPlace of
        head :: tail ->
            case GenLevel.tryToPlace (Boat pos head.size dir head.id) board.matrix of
                ( matrix, Just boat ) ->
                    let
                        newModel =
                            case turn of
                                Player ->
                                    { model
                                        | myBoard =
                                            { board
                                                | matrix = matrix
                                                , boatsToPlace = tail
                                                , boats = Dict.insert head.id boat board.boats
                                            }
                                    }

                                CPU ->
                                    { model
                                        | cpuBoard =
                                            { board
                                                | matrix = matrix
                                                , boatsToPlace = tail
                                                , boats = Dict.insert head.id boat board.boats
                                            }
                                    }
                    in
                    ( newModel
                    , GenLevel.computeAvailableCells matrix
                        |> GenLevel.randomizeBoatPlacements turn
                    )

                _ ->
                    ( model
                    , GenLevel.computeAvailableCells board.matrix
                        |> GenLevel.randomizeBoatPlacements turn
                    )

        [] ->
            ( model, Cmd.none )


mouseMoveOnMyBoard : ( Float, Float ) -> Model -> Model
mouseMoveOnMyBoard ( x, y ) model =
    let
        board =
            model.myBoard

        currentCell =
            Grid.getClosestCell { x = x, y = y } board.grid
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
    let
        board =
            model.cpuBoard

        cell =
            Grid.getClosestCell { x = x, y = y } board.grid
    in
    { model | cpuBoard = { board | cellOver = Just cell } }


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


getBoatById : String -> Model -> Maybe Boat
getBoatById id_ model =
    case Dict.Extra.find (\_ boat -> boat.id == id_) model.myBoard.boats of
        Just ( id, boat ) ->
            Just boat

        _ ->
            Nothing


mouseDownMyBoard : Mouse.Event -> Model -> Model
mouseDownMyBoard event model =
    let
        cell =
            Grid.getClosestCell model.currentMousePos model.myBoard.grid

        maybeBoat =
            getBoatByCell cell model
    in
    { model | clickedBoat = maybeBoat, clickedCell = Just cell }


mouseDownCpuBoard : Mouse.Event -> Model -> Model
mouseDownCpuBoard event model =
    { model
        | fire =
            model.fire
                |> Animator.go (Animator.millis 2000) (not <| Animator.current model.fire)
    }


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


mouseUpCpu : Mouse.Event -> Model -> Model
mouseUpCpu event model =
    { model
        | fire =
            model.fire
                |> Animator.go (Animator.millis 2000) (not <| Animator.current model.fire)
    }


mouseUp : String -> Mouse.Event -> Model -> Model
mouseUp boardId event model =
    case boardId of
        "myBoard" ->
            { model | clickedBoat = Nothing, clickedCell = Nothing }

        _ ->
            mouseUpCpu event model


pieceOver : String -> Model -> Model
pieceOver boatId model =
    { model | focusedBoat = getBoatById boatId model }


pieceOut : String -> Model -> Model
pieceOut boatId model =
    { model | focusedBoat = Nothing }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        -- case Debug.log "msg" msg of
        GetCoordAndDirection turn ( pos, dir ) ->
            iterPlacement turn pos dir model

        Generate turn ->
            let
                newModel =
                    initModel model turn
            in
            ( newModel, GenLevel.randomizeBoatPlacements turn <| GenLevel.createBoatStartCouples 0 0 9 9 )

        PieceOver boatId ->
            ( pieceOver boatId model, Cmd.none )

        PieceOut boatId ->
            ( pieceOut boatId model, Cmd.none )

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

        Blink ->
            ( { model
                | focusedUp =
                    model.focusedUp
                        |> Animator.go (Animator.millis 100) (not <| Animator.current model.focusedUp)
              }
            , Cmd.none
            )

        Tick newTime ->
            ( model
                |> Animator.update newTime animator
            , Cmd.none
            )


port requestSvgMousePos : ( String, Int, Int ) -> Cmd msg


port svgMousePosResult : (( String, Float, Float ) -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ svgMousePosResult SvgMousePosResult
        , Time.every 200 (always Blink)
        , animator
            |> Animator.toSubscription Tick model
        ]
