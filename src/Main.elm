port module Main exposing (main)

import Animator
import Animator.Inline
import Array
import Browser
import Color
import Dict exposing (Dict)
import Dict.Extra
import Element exposing (Element, alignRight, centerX, centerY, column, el, fill, padding, px, rgb, rgb255, row, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Figures
import GenLevel
import Grid exposing (drawGrid)
import Html exposing (Html, button, div, span, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Html.Events.Extra.Mouse as Mouse
import Matrix exposing (Matrix)
import Process
import Random
import Random.List
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)
import Task
import Time
import Types exposing (Board, CellType(..), CpuFireEngine, Direction(..), FloatCoord, Grid, GridCoord, GridSize, Model, Msg(..), Ship, ShipDef, State(..), Turn(..))


delay : Float -> Msg -> Cmd Msg
delay time msg =
    Process.sleep time
        |> Task.perform (\_ -> msg)


animator : Animator.Animator Model
animator =
    Animator.animator
        |> Animator.watching
            .firing
            (\newFiring model ->
                { model | firing = newFiring }
            )


shipDefs : List ShipDef
shipDefs =
    [ ShipDef "Carrier" 5
    , ShipDef "Battleship" 4
    , ShipDef "Cruiser" 3
    , ShipDef "Submarine" 3
    , ShipDef "Destroyer" 2
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


initModel : Turn -> Model -> Model
initModel turn model =
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
                , shipsToPlace = shipDefs
                , ships = Dict.empty
                , shots = Matrix.repeat 10 10 False
                , cellUnderMouse = Nothing
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
            , shipsToPlace = shipDefs
            , ships = Dict.empty
            , shots = Matrix.repeat 10 10 False
            , cellUnderMouse = Nothing
            , grid = Grid 10 10 10 1 2 30 { x = 20, y = 20 } "#A0A0A0"
            , id = "myBoard"
            }
      , cpuBoard =
            { matrix = createFreshMatrix
            , shipsToPlace = shipDefs
            , ships = Dict.empty
            , shots = Matrix.repeat 10 10 False
            , cellUnderMouse = Nothing
            , grid = Grid 10 10 10 1 2 30 { x = 20, y = 20 } "#A0A0A0"
            , id = "cpuBoard"
            }
      , currentMousePos = { x = 0, y = 0 }
      , clickedShip = Nothing
      , clickedCell = Nothing
      , clickedPos = { x = 0, y = 0 }
      , draggingShip = False
      , focusedShip = Nothing
      , firing = Animator.init False
      , firingCell = Nothing
      , state = Preparing
      , cpuFireEngine = CpuFireEngine []
      }
    , Cmd.batch
        [ GenLevel.randomizeShipPlacements Player <| GenLevel.createShipStartCouples 0 0 9 9
        , GenLevel.randomizeShipPlacements CPU <| GenLevel.createShipStartCouples 0 0 9 9
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


shipToSvg grid ship focusedShip model =
    case ( model.clickedShip, model.draggingShip ) of
        ( Just clickedShip, True ) ->
            if clickedShip.id == ship.id then
                clickedShipToSvg grid ship

            else
                regularShipToSvg grid ship focusedShip model

        _ ->
            regularShipToSvg grid ship focusedShip model


clickedShipToSvg grid ship =
    Figures.drawShipPlacement ship grid


computeGridSizeByDirection : Direction -> Int -> GridSize
computeGridSizeByDirection dir size =
    case dir of
        South ->
            { width = 1, height = size }

        East ->
            { width = size, height = 1 }

        North ->
            { width = 1, height = size }

        West ->
            { width = size, height = 1 }


regularShipToSvg grid ship focusedShip model =
    Figures.drawShip ship grid (model.state == Preparing && focusedShip == Just ship)


generateShipsSvg grid ships focusedShip model =
    ships
        |> Dict.values
        |> List.filter
            (\myShip ->
                case model.clickedShip of
                    Nothing ->
                        True

                    Just ship ->
                        (ship.id /= myShip.id) || isShipAllowedToBeThere model.myBoard myShip
            )
        |> List.map (\ship -> shipToSvg grid ship focusedShip model)


generatePhantomShip grid board model =
    case ( model.clickedShip, model.draggingShip ) of
        ( Just clickedShip, True ) ->
            let
                originShipTopLeft =
                    getGridTopLeftCoord clickedShip board.grid

                floatingTopLeft =
                    { x = originShipTopLeft.x - (model.clickedPos.x - model.currentMousePos.x)
                    , y = originShipTopLeft.y - (model.clickedPos.y - model.currentMousePos.y)
                    }
            in
            [ Figures.drawShipFloating clickedShip board.grid floatingTopLeft ]

        _ ->
            []


viewMyBoard model =
    let
        board =
            model.myBoard

        grid =
            board.grid

        svgShips =
            generateShipsSvg grid board.ships model.focusedShip model

        phantomShip =
            if model.state == Preparing then
                generatePhantomShip grid board model

            else
                []
    in
    svg
        [ id board.id
        , Svg.Attributes.width "350"
        , height "350"
        , Mouse.onMove (MouseMove board.id)
        , Mouse.onDown (MouseDown board.id)
        , Mouse.onUp (MouseUp board.id)
        ]
    <|
        List.concat [ [ drawGrid grid [] ], svgShips, viewShots model.myBoard, phantomShip, viewPlayerVeil model.state ]


belongsToShip board coord =
    getShipByCell coord board /= Nothing


viewShot board coord =
    if belongsToShip board coord then
        Figures.drawHit board.grid coord

    else
        Figures.drawMiss board.grid coord


viewShots board =
    board.shots
        |> Matrix.indexedMap (\col row value -> ( col, row, value ))
        |> Matrix.toArray
        |> Array.toList
        |> List.filter (\( _, _, value ) -> value)
        |> List.map (\( col, row, _ ) -> { col = col, row = row })
        |> List.map (viewShot board)


isAShotCoord : Matrix Bool -> GridCoord -> Bool
isAShotCoord shots coord =
    Matrix.get coord.col coord.row shots == Ok True


viewCpuVeil modelState =
    if modelState == Preparing || modelState == Playing CPU then
        [ rect
            [ x "0"
            , y "0"
            , Svg.Attributes.width "350"
            , Svg.Attributes.height "350"
            , Svg.Attributes.fill "#80808060"
            ]
            []
        ]

    else
        []


viewPlayerVeil modelState =
    if modelState == Playing Player then
        [ rect
            [ x "0"
            , y "0"
            , Svg.Attributes.width "350"
            , Svg.Attributes.height "350"
            , Svg.Attributes.fill "#80808060"
            ]
            []
        ]

    else
        []


viewCpuBoard model =
    let
        board =
            model.cpuBoard

        grid =
            board.grid

        cellUnderMouseSvg =
            let
                maybeCoord =
                    case model.firingCell of
                        Just firingCoord ->
                            Just firingCoord

                        Nothing ->
                            board.cellUnderMouse

                fireAmount =
                    Animator.linear model.firing <|
                        \state ->
                            if state then
                                Animator.at 1

                            else
                                Animator.at 0
            in
            case ( maybeCoord, model.state ) of
                ( Just coord, Playing Player ) ->
                    if isAShotCoord board.shots coord then
                        []

                    else
                        [ Figures.drawTarget
                            grid
                            coord
                            fireAmount
                        ]

                _ ->
                    []
    in
    svg
        [ id board.id
        , Svg.Attributes.width "350"
        , Svg.Attributes.height "350"
        , Mouse.onMove (MouseMove board.id)
        , Mouse.onDown (MouseDown board.id)
        , Mouse.onUp (MouseUp board.id)
        ]
    <|
        List.concat [ [ drawGrid grid [] ], viewShots model.cpuBoard, cellUnderMouseSvg, viewCpuVeil model.state ]


viewMe model =
    column
        []
        [ Element.html <| viewMyBoard model
        , el
            [ centerX ]
          <|
            Element.text "You"
        ]


viewCpu model =
    column
        []
        [ Element.html <| viewCpuBoard model
        , Element.el
            [ Element.centerX ]
          <|
            Element.text "Computer"
        ]


viewBoards model =
    row
        [ padding 40
        , Element.spacing 40
        ]
        [ viewMe model
        , viewCpu model
        ]


surroundCellByShots : GridCoord -> Board -> Board
surroundCellByShots { col, row } board =
    let
        writeSurroundedCellIfEmpty ( cellCol, cellRow ) matrix =
            Matrix.set cellCol cellRow True matrix

        newShots =
            board.shots
                |> writeSurroundedCellIfEmpty ( col - 1, row )
                |> writeSurroundedCellIfEmpty ( col - 1, row - 1 )
                |> writeSurroundedCellIfEmpty ( col, row - 1 )
                |> writeSurroundedCellIfEmpty ( col + 1, row - 1 )
                |> writeSurroundedCellIfEmpty ( col + 1, row )
                |> writeSurroundedCellIfEmpty ( col + 1, row + 1 )
                |> writeSurroundedCellIfEmpty ( col, row + 1 )
                |> writeSurroundedCellIfEmpty ( col - 1, row + 1 )
    in
    { board | shots = newShots }


surroundShipByShots : List GridCoord -> Board -> Board
surroundShipByShots shipCells board =
    List.foldl surroundCellByShots board shipCells


completeUnnecessaryShotsForShip : String -> Ship -> Board -> Board
completeUnnecessaryShotsForShip _ ship board =
    let
        shipCells =
            GenLevel.computeShipCellPositions ship
    in
    if List.all (isAShotCoord board.shots) shipCells then
        surroundShipByShots shipCells board

    else
        board


completeUnnecessaryShots : Board -> Board
completeUnnecessaryShots board =
    board.ships
        |> Dict.foldl completeUnnecessaryShotsForShip board


viewInformationMessage : Model -> Element Msg
viewInformationMessage model =
    let
        message =
            case model.state of
                Preparing ->
                    "You are invited to move your ships before launching the game"

                Playing Player ->
                    "Chose a cell to hit on your opponent's board"

                Playing CPU ->
                    "Wait for your opponent hit"

                End Player ->
                    "You win!"

                End CPU ->
                    "The computer wins!"
    in
    el
        [ centerX
        , centerY
        , Element.height <| px 40
        , Background.color <| rgb 0.9 0.9 0.9
        , Element.paddingXY 20 0
        , Font.size 14
        ]
    <|
        el
            [ centerY ]
        <|
            Element.text message


viewControlBar : Model -> Element Msg
viewControlBar model =
    case model.state of
        Preparing ->
            row
                [ centerX, Element.spacing 6 ]
                [ Input.button
                    [ Background.color <| Element.rgb 0.9 0.9 1
                    , padding 4
                    , Border.solid
                    , Border.width 1
                    ]
                    { label = Element.text "New random draw", onPress = Just <| Generate Player }
                , Input.button
                    [ Background.color <| Element.rgb 1 0.9 0.9
                    , padding 4
                    , Border.solid
                    , Border.width 1
                    ]
                    { label = Element.text "Let's play", onPress = Just <| Launch }
                ]

        End _ ->
            row
                [ centerX, Element.spacing 6 ]
                [ Input.button
                    [ Background.color <| Element.rgb 0.9 0.9 1
                    , padding 4
                    , Border.solid
                    , Border.width 1
                    ]
                    { label = Element.text "New game", onPress = Just <| NewGame }
                ]

        _ ->
            Element.text ""


view model =
    Element.layout
        []
    <|
        column
            [ Element.spacing 10, centerX ]
            [ viewBoards model
            , viewControlBar model
            , viewInformationMessage model
            ]


buildForbiddenCellsMatrix : Board -> Ship -> Matrix Bool
buildForbiddenCellsMatrix board exceptShip =
    let
        forbiddenMatrix =
            Matrix.repeat 10 10 False

        addSurroundingCells : GridCoord -> List GridCoord -> List GridCoord
        addSurroundingCells cell cells =
            List.concat
                [ cells
                , [ { col = cell.col - 1, row = cell.row - 1 }
                  , { col = cell.col, row = cell.row - 1 }
                  , { col = cell.col + 1, row = cell.row - 1 }
                  , { col = cell.col - 1, row = cell.row }
                  , { col = cell.col + 1, row = cell.row }
                  , { col = cell.col - 1, row = cell.row + 1 }
                  , { col = cell.col, row = cell.row + 1 }
                  , { col = cell.col + 1, row = cell.row + 1 }
                  ]
                ]

        addAllSurroundedCells : List GridCoord -> List GridCoord
        addAllSurroundedCells cells =
            let
                surroundingCells =
                    List.foldl addSurroundingCells [] cells
            in
            List.concat [ cells, surroundingCells ]

        fillMatrixWithForbiddenCells ship matrix =
            GenLevel.computeShipCellPositions ship
                |> addAllSurroundedCells
                |> List.foldl (\coord matrix_ -> Matrix.set coord.col coord.row True matrix_) matrix
    in
    board.ships
        |> Dict.values
        |> List.filter (\ship -> ship.id /= exceptShip.id)
        |> List.foldl fillMatrixWithForbiddenCells forbiddenMatrix


isShipAllowedToBeThere : Board -> Ship -> Bool
isShipAllowedToBeThere board ship =
    let
        forbiddenMatrix =
            buildForbiddenCellsMatrix board ship

        shipCells =
            GenLevel.computeShipCellPositions ship

        collidingCells =
            shipCells
                |> List.filter (\coord -> Matrix.get coord.col coord.row forbiddenMatrix == Ok True)

        outOfScreenCells =
            shipCells
                |> List.filter
                    (\coord ->
                        coord.col
                            < 0
                            || coord.col
                            >= board.grid.colCount
                            || coord.row
                            < 0
                            || coord.row
                            >= board.grid.rowCount
                    )
    in
    List.length collidingCells == 0 && List.length outOfScreenCells == 0


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
    case board.shipsToPlace of
        head :: tail ->
            case GenLevel.tryToPlace (Ship pos head.size dir head.id) board.matrix of
                ( matrix, Just ship ) ->
                    let
                        newModel =
                            case turn of
                                Player ->
                                    { model
                                        | myBoard =
                                            { board
                                                | matrix = matrix
                                                , shipsToPlace = tail
                                                , ships = Dict.insert head.id ship board.ships
                                            }
                                    }

                                CPU ->
                                    { model
                                        | cpuBoard =
                                            { board
                                                | matrix = matrix
                                                , shipsToPlace = tail
                                                , ships = Dict.insert head.id ship board.ships
                                            }
                                    }
                    in
                    ( newModel
                    , GenLevel.computeAvailableCells matrix
                        |> GenLevel.randomizeShipPlacements turn
                    )

                _ ->
                    ( model
                    , GenLevel.computeAvailableCells board.matrix
                        |> GenLevel.randomizeShipPlacements turn
                    )

        [] ->
            ( model, Cmd.none )


mouseMoveOnMyBoard : ( Float, Float ) -> Model -> Model
mouseMoveOnMyBoard ( x, y ) model =
    case ( model.clickedShip, model.clickedCell, model.draggingShip ) of
        ( Just ship, Just clickedCell, True ) ->
            let
                board =
                    model.myBoard

                grid =
                    board.grid

                shipBoundingBox =
                    Figures.computeGridSizeByDirection ship

                currentCell =
                    Grid.getClosestCell { x = x, y = y } grid

                rectifyPos oldShip =
                    let
                        row =
                            case oldShip.dir of
                                North ->
                                    if oldShip.pos.row - oldShip.size < 0 then
                                        oldShip.size - 1

                                    else if oldShip.pos.row >= grid.rowCount then
                                        grid.rowCount - 1

                                    else
                                        oldShip.pos.row

                                South ->
                                    if oldShip.pos.row < 0 then
                                        0

                                    else if oldShip.pos.row + oldShip.size >= grid.rowCount then
                                        grid.rowCount - oldShip.size

                                    else
                                        oldShip.pos.row

                                East ->
                                    oldShip.pos.row

                                West ->
                                    oldShip.pos.row

                        col =
                            case oldShip.dir of
                                West ->
                                    if oldShip.pos.col - oldShip.size < 0 then
                                        oldShip.size - 1

                                    else if oldShip.pos.col >= grid.colCount then
                                        grid.colCount - 1

                                    else
                                        oldShip.pos.col

                                East ->
                                    if oldShip.pos.col < 0 then
                                        0

                                    else if oldShip.pos.col + oldShip.size >= grid.colCount then
                                        grid.colCount - oldShip.size

                                    else
                                        oldShip.pos.col

                                North ->
                                    oldShip.pos.col

                                South ->
                                    oldShip.pos.col

                        newPos =
                            { row = row, col = col }
                    in
                    { ship | pos = newPos }

                newShip =
                    { ship
                        | pos =
                            { col = ship.pos.col + currentCell.col - clickedCell.col
                            , row = ship.pos.row + currentCell.row - clickedCell.row
                            }
                    }
                        |> rectifyPos

                newBoards =
                    { board | ships = Dict.insert ship.id newShip board.ships }
            in
            { model | myBoard = newBoards }

        _ ->
            if (model.clickedCell /= Nothing) && tearoffDrag { x = x, y = y } model.clickedPos then
                { model | draggingShip = True }

            else
                model


tearoffDrag : FloatCoord -> FloatCoord -> Bool
tearoffDrag coord1 coord2 =
    let
        p1 =
            coord1.x - coord2.x

        p2 =
            coord1.y - coord2.y
    in
    sqrt (p1 * p1 + p2 * p2) > 5.0


mouseMoveOnCPUBoard : ( Float, Float ) -> Model -> Model
mouseMoveOnCPUBoard ( x, y ) model =
    let
        board =
            model.cpuBoard

        newCpuBoard =
            { board | cellUnderMouse = Grid.getCellUnder { x = x, y = y } board.grid }
    in
    { model | cpuBoard = newCpuBoard }


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


isCellBelongToShip : GridCoord -> Ship -> Bool
isCellBelongToShip cell ship =
    GenLevel.computeShipCellPositions ship
        |> List.any (\shipCell -> shipCell == cell)


getShipByCell : GridCoord -> Board -> Maybe Ship
getShipByCell cell board =
    case Dict.Extra.find (\_ ship -> isCellBelongToShip cell ship) board.ships of
        Just ( id, ship ) ->
            Just ship

        _ ->
            Nothing


getShipById : String -> Board -> Maybe Ship
getShipById id_ board =
    case Dict.Extra.find (\_ ship -> ship.id == id_) board.ships of
        Just ( _, ship ) ->
            Just ship

        _ ->
            Nothing


getGridTopLeftCoord ship grid =
    let
        topLeft =
            case ship.dir of
                South ->
                    ship.pos

                East ->
                    ship.pos

                North ->
                    { col = ship.pos.col, row = ship.pos.row - ship.size + 1 }

                West ->
                    { col = ship.pos.col - ship.size + 1, row = ship.pos.row }
    in
    Grid.getCellCoord topLeft.col topLeft.row grid


mouseDownMyBoard : Mouse.Event -> Model -> Model
mouseDownMyBoard event model =
    case model.state of
        Preparing ->
            let
                cell =
                    Grid.getClosestCell model.currentMousePos model.myBoard.grid

                maybeShip =
                    getShipByCell cell model.myBoard
            in
            { model
                | clickedShip = maybeShip
                , clickedCell = Just cell
                , clickedPos = model.currentMousePos
            }

        _ ->
            model


mouseDownCpuBoard : Mouse.Event -> Model -> Model
mouseDownCpuBoard event model =
    let
        cell =
            Grid.getClosestCell model.currentMousePos model.cpuBoard.grid
    in
    if model.state /= Playing Player then
        model

    else if isAShotCoord model.cpuBoard.shots cell then
        model

    else
        { model
            | firing =
                model.firing
                    |> Animator.go (Animator.millis 1000) (not <| Animator.current model.firing)
            , firingCell = Just cell
        }


cancelMoveIfNeeded : Model -> Model
cancelMoveIfNeeded model =
    case model.clickedShip of
        Just clickedShip ->
            case getShipById clickedShip.id model.myBoard of
                Just movedShip ->
                    if isShipAllowedToBeThere model.myBoard movedShip then
                        model

                    else
                        let
                            board =
                                model.myBoard

                            newBoard =
                                { board | ships = Dict.insert movedShip.id clickedShip board.ships }
                        in
                        { model | myBoard = newBoard }

                Nothing ->
                    model

        Nothing ->
            model


rotateShip90 : Ship -> Ship
rotateShip90 ship =
    case ship.dir of
        North ->
            { ship | dir = East }

        East ->
            { ship | dir = South }

        South ->
            { ship | dir = West }

        West ->
            { ship | dir = North }


centralRotation : Ship -> Ship
centralRotation ship =
    let
        halfSize =
            ship.size // 2
    in
    case ship.dir of
        North ->
            { ship | dir = East, pos = { col = ship.pos.col - halfSize, row = ship.pos.row - halfSize } }

        East ->
            { ship | dir = South, pos = { col = ship.pos.col + halfSize, row = ship.pos.row - halfSize } }

        South ->
            { ship | dir = West, pos = { col = ship.pos.col + halfSize, row = ship.pos.row + halfSize } }

        West ->
            { ship | dir = North, pos = { col = ship.pos.col - halfSize, row = ship.pos.row + halfSize } }


reverseDirShip : Ship -> Ship
reverseDirShip ship =
    case ship.dir of
        North ->
            { ship
                | dir = South
                , pos =
                    { col = ship.pos.col
                    , row = ship.pos.row - ship.size + 1
                    }
            }

        East ->
            { ship
                | dir = West
                , pos =
                    { col = ship.pos.col + ship.size - 1
                    , row = ship.pos.row
                    }
            }

        South ->
            { ship
                | dir = North
                , pos =
                    { col = ship.pos.col
                    , row = ship.pos.row + ship.size - 1
                    }
            }

        West ->
            { ship
                | dir = East
                , pos =
                    { col = ship.pos.col - ship.size + 1
                    , row = ship.pos.row
                    }
            }


rotationStuff : Model -> Model
rotationStuff model =
    let
        cell =
            Grid.getClosestCell model.currentMousePos model.myBoard.grid
    in
    if not model.draggingShip && Just cell == model.clickedCell then
        case model.clickedShip of
            Just clickedShip ->
                let
                    ship90 =
                        clickedShip |> rotateShip90

                    ship270 =
                        ship90 |> rotateShip90 |> rotateShip90

                    reverse90 =
                        clickedShip |> reverseDirShip |> rotateShip90

                    reverse270 =
                        reverse90 |> rotateShip90 |> rotateShip90

                    central =
                        clickedShip |> centralRotation

                    newShip =
                        if isShipAllowedToBeThere board ship90 then
                            ship90

                        else if isShipAllowedToBeThere board ship270 then
                            ship270

                        else if isShipAllowedToBeThere board reverse90 then
                            reverse90

                        else if isShipAllowedToBeThere board reverse270 then
                            reverse270

                        else if isShipAllowedToBeThere board central then
                            central

                        else
                            clickedShip

                    board =
                        model.myBoard

                    newBoard =
                        { board | ships = Dict.insert newShip.id newShip board.ships }
                in
                { model | myBoard = newBoard }

            Nothing ->
                model

    else
        model


cleanupMoveModelItems : Model -> Model
cleanupMoveModelItems model =
    { model
        | clickedShip = Nothing
        , clickedCell = Nothing
        , draggingShip = False
    }


mouseUpMyBoard : Model -> Model
mouseUpMyBoard model =
    case model.state of
        Preparing ->
            model
                |> cancelMoveIfNeeded
                |> rotationStuff
                |> cleanupMoveModelItems

        _ ->
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
    case Animator.current model.firing of
        False ->
            model

        _ ->
            { model
                | firing =
                    model.firing
                        |> Animator.go (Animator.millis 1000) (not <| Animator.current model.firing)
                , firingCell = Nothing
            }


mouseUp : String -> Mouse.Event -> Model -> Model
mouseUp boardId event model =
    case boardId of
        "myBoard" ->
            mouseUpMyBoard model

        _ ->
            mouseUpCpu event model


pieceOver : String -> Model -> Model
pieceOver shipId model =
    { model | focusedShip = getShipById shipId model.myBoard }


pieceOut : String -> Model -> Model
pieceOut shipId model =
    { model | focusedShip = Nothing }


playerFire : Model -> Model
playerFire model =
    let
        board =
            model.cpuBoard

        ( newBoard, nextTurn ) =
            case model.firingCell of
                Just coord ->
                    ( { board | shots = Matrix.set coord.col coord.row True board.shots }
                        |> completeUnnecessaryShots
                    , if belongsToShip board coord then
                        Player

                      else
                        CPU
                    )

                _ ->
                    ( board, Player )
    in
    { model | cpuBoard = newBoard, state = Playing nextTurn }
        |> checkForEnd


getShotRandomCandidateCells : Board -> List GridCoord
getShotRandomCandidateCells board =
    board.shots
        |> Matrix.indexedMap (\col row value -> ( { col = col, row = row }, not value ))
        |> Matrix.toArray
        |> Array.toList
        |> List.filter (\( _, value ) -> value)
        |> List.map (\( coord, _ ) -> coord)


getAllShipShots : Ship -> Matrix Bool -> List GridCoord
getAllShipShots ship shots =
    GenLevel.computeShipCellPositions ship
        |> List.filter (\cellCoord -> isAShotCoord shots cellCoord)


isUnfinishedShip : Ship -> Board -> Bool
isUnfinishedShip ship board =
    let
        shipCells =
            GenLevel.computeShipCellPositions ship

        shipLen =
            List.length shipCells

        successfulShots =
            getAllShipShots ship board.shots

        successfulLen =
            List.length successfulShots
    in
    successfulLen < shipLen && successfulLen > 0


findUnfinishedShip : Board -> Maybe Ship
findUnfinishedShip board =
    let
        unfinishedShips =
            board.ships
                |> Dict.filter (\_ ship -> isUnfinishedShip ship board)
                |> Dict.values
    in
    case unfinishedShips of
        head :: _ ->
            Just head

        [] ->
            Nothing


getShotsForSingleCell : Ship -> GridCoord -> Model -> List GridCoord
getShotsForSingleCell ship { col, row } model =
    let
        shots =
            model.myBoard.shots

        addCellIfEmpty coord cells =
            if Matrix.get coord.col coord.row shots == Ok False then
                coord :: cells

            else
                cells
    in
    []
        |> addCellIfEmpty { col = col - 1, row = row }
        |> addCellIfEmpty { col = col, row = row - 1 }
        |> addCellIfEmpty { col = col + 1, row = row }
        |> addCellIfEmpty { col = col, row = row + 1 }


getShotsForMultiCell : Ship -> ( GridCoord, GridCoord ) -> Model -> List GridCoord
getShotsForMultiCell ship ( coord1, coord2 ) model =
    let
        shots =
            model.myBoard.shots

        maybeCandidate1 =
            case ship.dir of
                North ->
                    if Matrix.get coord1.col (coord1.row - 1) shots == Ok False then
                        Just { col = coord1.col, row = coord1.row - 1 }

                    else
                        Nothing

                South ->
                    if Matrix.get coord1.col (coord1.row - 1) shots == Ok False then
                        Just { col = coord1.col, row = coord1.row - 1 }

                    else
                        Nothing

                East ->
                    if Matrix.get (coord1.col - 1) coord1.row shots == Ok False then
                        Just { col = coord1.col - 1, row = coord1.row }

                    else
                        Nothing

                West ->
                    if Matrix.get (coord1.col - 1) coord1.row shots == Ok False then
                        Just { col = coord1.col - 1, row = coord1.row }

                    else
                        Nothing

        maybeCandidate2 =
            case ship.dir of
                North ->
                    if Matrix.get coord2.col (coord2.row + 1) shots == Ok False then
                        Just { col = coord2.col, row = coord2.row + 1 }

                    else
                        Nothing

                South ->
                    if Matrix.get coord2.col (coord2.row + 1) shots == Ok False then
                        Just { col = coord2.col, row = coord2.row + 1 }

                    else
                        Nothing

                East ->
                    if Matrix.get (coord2.col + 1) coord2.row shots == Ok False then
                        Just { col = coord2.col + 1, row = coord2.row }

                    else
                        Nothing

                West ->
                    if Matrix.get (coord2.col + 1) coord2.row shots == Ok False then
                        Just { col = coord2.col + 1, row = coord2.row }

                    else
                        Nothing
    in
    [ maybeCandidate1, maybeCandidate2 ]
        |> List.foldr
            (\maybeCand res ->
                case maybeCand of
                    Nothing ->
                        res

                    Just cand ->
                        cand :: res
            )
            []


getShotsForUnfinishedShip : Ship -> Model -> List GridCoord
getShotsForUnfinishedShip ship model =
    let
        shotCoords =
            getAllShipShots ship model.myBoard.shots

        getDistance : GridCoord -> GridCoord -> Int
        getDistance coord1 coord2 =
            let
                a =
                    coord1.col - coord2.col

                b =
                    coord1.row - coord2.row
            in
            a * a + b * b

        createContinuousBinomes gridCoord ( binomes, maybeBinome ) =
            case maybeBinome of
                Nothing ->
                    ( binomes, Just ( gridCoord, gridCoord ) )

                Just ( coord1, coord2 ) ->
                    if getDistance coord2 gridCoord == 1 then
                        ( binomes, Just ( coord1, gridCoord ) )

                    else
                        ( ( coord1, coord2 ) :: binomes, Just ( gridCoord, gridCoord ) )

        sortedCoords =
            case ship.dir of
                North ->
                    List.sortBy .row shotCoords

                South ->
                    List.sortBy .row shotCoords

                East ->
                    List.sortBy .col shotCoords

                West ->
                    List.sortBy .col shotCoords

        continuousBinomes =
            case List.foldl createContinuousBinomes ( [], Nothing ) sortedCoords of
                ( binomes, Nothing ) ->
                    binomes

                ( binomes, Just binome ) ->
                    binome :: binomes
    in
    case continuousBinomes of
        [] ->
            []

        head :: _ ->
            case getDistance (Tuple.first head) (Tuple.second head) of
                0 ->
                    getShotsForSingleCell ship (Tuple.first head) model

                _ ->
                    getShotsForMultiCell ship head model


cpuFire : Model -> ( Model, Cmd Msg )
cpuFire model =
    let
        candidateCells =
            case findUnfinishedShip model.myBoard of
                Just unfinishedShip ->
                    getShotsForUnfinishedShip unfinishedShip model

                _ ->
                    getShotRandomCandidateCells model.myBoard
    in
    case candidateCells of
        head :: tail ->
            let
                coordGenerator : Random.Generator GridCoord
                coordGenerator =
                    Random.uniform head tail
            in
            ( model, Random.generate GetCellCandidate coordGenerator )

        [] ->
            ( model, Cmd.none )


isShipSunk : Matrix Bool -> Ship -> Bool
isShipSunk shots ship =
    let
        shipShots =
            getAllShipShots ship shots
    in
    List.length shipShots == ship.size


areAllShipsSunk : Board -> Bool
areAllShipsSunk board =
    Dict.values board.ships
        |> List.all (isShipSunk board.shots)


checkForEnd : Model -> Model
checkForEnd model =
    if areAllShipsSunk model.myBoard then
        { model | state = End CPU }

    else if areAllShipsSunk model.cpuBoard then
        { model | state = End Player }

    else
        model


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        -- case Debug.log "msg" msg of
        GetCoordAndDirection turn ( pos, dir ) ->
            iterPlacement turn pos dir model

        NewGame ->
            let
                newModel =
                    model
                        |> initModel Player
                        |> initModel CPU
            in
            ( { newModel | state = Preparing }
            , Cmd.batch
                [ GenLevel.randomizeShipPlacements Player <| GenLevel.createShipStartCouples 0 0 9 9
                , GenLevel.randomizeShipPlacements CPU <| GenLevel.createShipStartCouples 0 0 9 9
                ]
            )

        Generate turn ->
            let
                newModel =
                    initModel turn model
            in
            ( newModel, GenLevel.randomizeShipPlacements turn <| GenLevel.createShipStartCouples 0 0 9 9 )

        Launch ->
            ( { model | state = Playing Player }, Cmd.none )

        PieceOver shipId ->
            ( pieceOver shipId model, Cmd.none )

        PieceOut shipId ->
            ( pieceOut shipId model, Cmd.none )

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

        PlayCPU ->
            cpuFire model

        GetCellCandidate cellCoord ->
            let
                board =
                    model.myBoard

                isBelongToShip =
                    belongsToShip model.myBoard cellCoord

                newBoard =
                    { board | shots = Matrix.set cellCoord.col cellCoord.row True board.shots }
                        |> completeUnnecessaryShots

                newModel =
                    { model
                        | myBoard = newBoard
                        , state =
                            if isBelongToShip then
                                Playing CPU

                            else
                                Playing Player
                    }
                        |> checkForEnd
            in
            ( newModel
            , if isBelongToShip && newModel.state == Playing CPU then
                delay 1000 PlayCPU

              else
                Cmd.none
            )

        Tick newTime ->
            let
                animModel =
                    model
                        |> Animator.update newTime animator

                ( newModel, cmd ) =
                    if Animator.previous animModel.firing && not (Animator.previous model.firing) then
                        let
                            afterFireModel =
                                playerFire animModel
                        in
                        if afterFireModel.state == Playing CPU then
                            ( afterFireModel, delay 1000 PlayCPU )

                        else
                            ( afterFireModel, Cmd.none )

                    else
                        ( animModel, Cmd.none )
            in
            ( newModel, cmd )


port requestSvgMousePos : ( String, Int, Int ) -> Cmd msg


port svgMousePosResult : (( String, Float, Float ) -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ svgMousePosResult SvgMousePosResult
        , animator
            |> Animator.toSubscription Tick model
        ]
