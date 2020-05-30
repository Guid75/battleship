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
import Types exposing (Board, Boat, BoatDef, CellType(..), Direction(..), FloatCoord, Grid, GridCoord, GridSize, Model, Msg(..), Turn(..))


animator : Animator.Animator Model
animator =
    Animator.animator
        |> Animator.watching
            .focusedUp
            (\newFocusedUp model ->
                { model | focusedUp = newFocusedUp }
            )
        |> Animator.watching
            .firing
            (\newFiring model ->
                { model | firing = newFiring }
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
            , grid = Grid 10 10 10 1 2 30 { x = 20, y = 20 } "#000000"
            , id = "myBoard"
            }
      , cpuBoard =
            { matrix = createFreshMatrix
            , boatsToPlace = boatDefs
            , boats = Dict.empty
            , shots = []
            , cellOver = Nothing
            , grid = Grid 10 10 10 1 2 30 { x = 20, y = 20 } "#000000"
            , id = "cpuBoard"
            }
      , currentMousePos = { x = 0, y = 0 }
      , clickedBoat = Nothing
      , clickedCell = Nothing
      , clickedPos = { x = 0, y = 0 }
      , draggingBoat = False
      , focusedBoat = Nothing
      , focusedUp = Animator.init False
      , firing = Animator.init False
      , firingCell = Nothing
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
    case ( model.clickedBoat, model.draggingBoat ) of
        ( Just clickedBoat, True ) ->
            if clickedBoat.id == boat.id then
                clickedBoatToSvg grid boat

            else
                regularBoatToSvg grid boat focusedBoat model

        _ ->
            regularBoatToSvg grid boat focusedBoat model


clickedBoatToSvg grid boat =
    Figures.drawBoatPlacement boat grid


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


regularBoatToSvg grid boat focusedBoat model =
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
    Figures.drawBoat boat grid attrs


generateBoatsSvg grid boats focusedBoat model =
    boats
        |> Dict.values
        |> List.filter
            (\myBoat ->
                case model.clickedBoat of
                    Nothing ->
                        True

                    Just boat ->
                        (boat.id /= myBoat.id) || isBoatAllowedToBeThere model.myBoard myBoat
            )
        |> List.map (\boat -> boatToSvg grid boat focusedBoat model)


generatePhantomBoat grid board model =
    case ( model.clickedBoat, model.draggingBoat ) of
        ( Just clickedBoat, True ) ->
            let
                originBoatTopLeft =
                    getGridTopLeftCoord clickedBoat board.grid

                floatingTopLeft =
                    { x = originBoatTopLeft.x - (model.clickedPos.x - model.currentMousePos.x)
                    , y = originBoatTopLeft.y - (model.clickedPos.y - model.currentMousePos.y)
                    }
            in
            [ Figures.drawBoatFloating clickedBoat board.grid floatingTopLeft ]

        _ ->
            []


viewMyBoard model =
    let
        board =
            model.myBoard

        grid =
            board.grid

        svgBoats =
            generateBoatsSvg grid board.boats model.focusedBoat model

        phantomBoat =
            generatePhantomBoat grid board model
    in
    svg
        [ id board.id
        , Svg.Attributes.width "400"
        , height "350"
        , viewBox "0 0 400 350"
        , Mouse.onMove (MouseMove board.id)
        , Mouse.onDown (MouseDown board.id)
        , Mouse.onUp (MouseUp board.id)
        ]
    <|
        List.concat [ [ drawGrid grid [] ], svgBoats, phantomBoat ]


isHit board coord =
    getBoatByCell coord board /= Nothing


viewShot board coord =
    if isHit board coord then
        Figures.drawHit board.grid coord

    else
        Figures.drawMiss board.grid coord


viewShots board =
    board.shots
        |> List.map (viewShot board)


viewCpuBoard model =
    let
        board =
            model.cpuBoard

        grid =
            board.grid

        cellOverSvg =
            let
                maybeCoord =
                    case model.firingCell of
                        Just firingCoord ->
                            Just firingCoord

                        Nothing ->
                            board.cellOver

                fireAmount =
                    Animator.linear model.firing <|
                        \state ->
                            if state then
                                Animator.at 1

                            else
                                Animator.at 0
            in
            case maybeCoord of
                Just coord ->
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
        , Svg.Attributes.width "400"
        , height "350"

        --        , viewBox "0 0 400 350"
        , Mouse.onMove (MouseMove board.id)
        , Mouse.onDown (MouseDown board.id)
        , Mouse.onUp (MouseUp board.id)
        ]
    <|
        List.concat [ [ drawGrid grid [] ], viewShots model.cpuBoard, cellOverSvg ]


viewMe model =
    column
        []
        [ Element.html <| viewMyBoard model
        , Element.el
            [ Element.centerX ]
          <|
            Element.text "Me"
        ]


viewCpu model =
    column
        []
        [ Element.html <| viewCpuBoard model
        , Element.el
            [ Element.centerX ]
          <|
            Element.text "CPU"
        ]


viewBoards model =
    row
        [ padding 40, Element.spacing 40 ]
        [ viewMe model
        , viewCpu model
        ]


view model =
    Element.layout
        []
    <|
        column
            []
            [ viewBoards model
            , Element.html <| button [ Html.Events.onClick <| Generate Player ] [ Html.text "New random draw" ]
            ]


buildForbiddenCellsMatrix : Board -> Boat -> Matrix Bool
buildForbiddenCellsMatrix board exceptBoat =
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

        fillMatrixWithForbiddenCells boat matrix =
            GenLevel.computeBoatCellPositions boat
                |> addAllSurroundedCells
                |> List.foldl (\coord matrix_ -> Matrix.set coord.col coord.row True matrix_) matrix
    in
    board.boats
        |> Dict.values
        |> List.filter (\boat -> boat.id /= exceptBoat.id)
        |> List.foldl fillMatrixWithForbiddenCells forbiddenMatrix


isBoatAllowedToBeThere : Board -> Boat -> Bool
isBoatAllowedToBeThere board boat =
    let
        forbiddenMatrix =
            buildForbiddenCellsMatrix board boat

        boatCells =
            GenLevel.computeBoatCellPositions boat

        collidingCells =
            boatCells
                |> List.filter (\coord -> Matrix.get coord.col coord.row forbiddenMatrix == Ok True)

        outOfScreenCells =
            boatCells
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
    case ( model.clickedBoat, model.clickedCell, model.draggingBoat ) of
        ( Just boat, Just clickedCell, True ) ->
            let
                board =
                    model.myBoard

                grid =
                    board.grid

                boatBoundingBox =
                    Figures.computeGridSizeByDirection boat

                currentCell =
                    Grid.getClosestCell { x = x, y = y } grid

                rectifyPos oldBoat =
                    let
                        row =
                            case oldBoat.dir of
                                North ->
                                    if oldBoat.pos.row - oldBoat.size < 0 then
                                        oldBoat.size - 1

                                    else if oldBoat.pos.row >= grid.rowCount then
                                        grid.rowCount - 1

                                    else
                                        oldBoat.pos.row

                                South ->
                                    if oldBoat.pos.row < 0 then
                                        0

                                    else if oldBoat.pos.row + oldBoat.size >= grid.rowCount then
                                        grid.rowCount - oldBoat.size

                                    else
                                        oldBoat.pos.row

                                East ->
                                    oldBoat.pos.row

                                West ->
                                    oldBoat.pos.row

                        col =
                            case oldBoat.dir of
                                West ->
                                    if oldBoat.pos.col - oldBoat.size < 0 then
                                        oldBoat.size - 1

                                    else if oldBoat.pos.col >= grid.colCount then
                                        grid.colCount - 1

                                    else
                                        oldBoat.pos.col

                                East ->
                                    if oldBoat.pos.col < 0 then
                                        0

                                    else if oldBoat.pos.col + oldBoat.size >= grid.colCount then
                                        grid.colCount - oldBoat.size

                                    else
                                        oldBoat.pos.col

                                North ->
                                    oldBoat.pos.col

                                South ->
                                    oldBoat.pos.col

                        newPos =
                            { row = row, col = col }
                    in
                    { boat | pos = newPos }

                newBoat =
                    { boat
                        | pos =
                            { col = boat.pos.col + currentCell.col - clickedCell.col
                            , row = boat.pos.row + currentCell.row - clickedCell.row
                            }
                    }
                        |> rectifyPos

                newBoards =
                    { board | boats = Dict.insert boat.id newBoat board.boats }
            in
            { model | myBoard = newBoards }

        _ ->
            if (model.clickedCell /= Nothing) && tearoffDrag { x = x, y = y } model.clickedPos then
                { model | draggingBoat = True }

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


getBoatByCell : GridCoord -> Board -> Maybe Boat
getBoatByCell cell board =
    case Dict.Extra.find (\_ boat -> isCellBelongToBoat cell boat) board.boats of
        Just ( id, boat ) ->
            Just boat

        _ ->
            Nothing


getBoatById : String -> Board -> Maybe Boat
getBoatById id_ board =
    case Dict.Extra.find (\_ boat -> boat.id == id_) board.boats of
        Just ( id, boat ) ->
            Just boat

        _ ->
            Nothing


getGridTopLeftCoord boat grid =
    let
        topLeft =
            case boat.dir of
                South ->
                    boat.pos

                East ->
                    boat.pos

                North ->
                    { col = boat.pos.col, row = boat.pos.row - boat.size + 1 }

                West ->
                    { col = boat.pos.col - boat.size + 1, row = boat.pos.row }
    in
    Grid.getCellCoord topLeft.col topLeft.row grid


mouseDownMyBoard : Mouse.Event -> Model -> Model
mouseDownMyBoard event model =
    let
        cell =
            Grid.getClosestCell model.currentMousePos model.myBoard.grid

        maybeBoat =
            getBoatByCell cell model.myBoard
    in
    { model
        | clickedBoat = maybeBoat
        , clickedCell = Just cell
        , clickedPos = model.currentMousePos
    }


mouseDownCpuBoard : Mouse.Event -> Model -> Model
mouseDownCpuBoard event model =
    let
        cell =
            Grid.getClosestCell model.currentMousePos model.cpuBoard.grid
    in
    { model
        | firing =
            model.firing
                |> Animator.go (Animator.millis 1000) (not <| Animator.current model.firing)
        , firingCell = Just cell
    }


cancelMoveIfNeeded : Model -> Model
cancelMoveIfNeeded model =
    case model.clickedBoat of
        Just clickedBoat ->
            case getBoatById clickedBoat.id model.myBoard of
                Just movedBoat ->
                    if isBoatAllowedToBeThere model.myBoard movedBoat then
                        model

                    else
                        let
                            board =
                                model.myBoard

                            newBoard =
                                { board | boats = Dict.insert movedBoat.id clickedBoat board.boats }
                        in
                        { model | myBoard = newBoard }

                Nothing ->
                    model

        Nothing ->
            model


rotateBoat90 : Boat -> Boat
rotateBoat90 boat =
    case boat.dir of
        North ->
            { boat | dir = East }

        East ->
            { boat | dir = South }

        South ->
            { boat | dir = West }

        West ->
            { boat | dir = North }


centralRotation : Boat -> Boat
centralRotation boat =
    let
        halfSize =
            boat.size // 2
    in
    case boat.dir of
        North ->
            { boat | dir = East, pos = { col = boat.pos.col - halfSize, row = boat.pos.row - halfSize } }

        East ->
            { boat | dir = South, pos = { col = boat.pos.col + halfSize, row = boat.pos.row - halfSize } }

        South ->
            { boat | dir = West, pos = { col = boat.pos.col + halfSize, row = boat.pos.row + halfSize } }

        West ->
            { boat | dir = North, pos = { col = boat.pos.col - halfSize, row = boat.pos.row + halfSize } }


reverseDirBoat : Boat -> Boat
reverseDirBoat boat =
    case boat.dir of
        North ->
            { boat
                | dir = South
                , pos =
                    { col = boat.pos.col
                    , row = boat.pos.row - boat.size + 1
                    }
            }

        East ->
            { boat
                | dir = West
                , pos =
                    { col = boat.pos.col + boat.size - 1
                    , row = boat.pos.row
                    }
            }

        South ->
            { boat
                | dir = North
                , pos =
                    { col = boat.pos.col
                    , row = boat.pos.row + boat.size - 1
                    }
            }

        West ->
            { boat
                | dir = East
                , pos =
                    { col = boat.pos.col - boat.size + 1
                    , row = boat.pos.row
                    }
            }


rotationStuff : Model -> Model
rotationStuff model =
    let
        cell =
            Grid.getClosestCell model.currentMousePos model.myBoard.grid
    in
    if not model.draggingBoat && Just cell == model.clickedCell then
        case model.clickedBoat of
            Just clickedBoat ->
                let
                    boat90 =
                        clickedBoat |> rotateBoat90

                    boat270 =
                        boat90 |> rotateBoat90 |> rotateBoat90

                    reverse90 =
                        clickedBoat |> reverseDirBoat |> rotateBoat90

                    reverse270 =
                        reverse90 |> rotateBoat90 |> rotateBoat90

                    central =
                        clickedBoat |> centralRotation

                    newBoat =
                        if isBoatAllowedToBeThere board boat90 then
                            boat90

                        else if isBoatAllowedToBeThere board boat270 then
                            boat270

                        else if isBoatAllowedToBeThere board reverse90 then
                            reverse90

                        else if isBoatAllowedToBeThere board reverse270 then
                            reverse270

                        else if isBoatAllowedToBeThere board central then
                            central

                        else
                            clickedBoat

                    board =
                        model.myBoard

                    newBoard =
                        { board | boats = Dict.insert newBoat.id newBoat board.boats }
                in
                { model | myBoard = newBoard }

            Nothing ->
                model

    else
        model


cleanupMoveModelItems : Model -> Model
cleanupMoveModelItems model =
    { model
        | clickedBoat = Nothing
        , clickedCell = Nothing
        , draggingBoat = False
    }


mouseUpMyBoard : Model -> Model
mouseUpMyBoard model =
    model
        |> cancelMoveIfNeeded
        |> rotationStuff
        |> cleanupMoveModelItems


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
pieceOver boatId model =
    { model | focusedBoat = getBoatById boatId model.myBoard }


pieceOut : String -> Model -> Model
pieceOut boatId model =
    { model | focusedBoat = Nothing }


fire : Model -> Model
fire model =
    let
        board =
            model.cpuBoard

        newBoard =
            case model.firingCell of
                Just coord ->
                    { board | shots = coord :: board.shots }

                _ ->
                    board
    in
    { model | cpuBoard = newBoard }


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
            let
                animModel =
                    model
                        |> Animator.update newTime animator

                newModel =
                    if Animator.previous animModel.firing && not (Animator.previous model.firing) then
                        fire animModel

                    else
                        animModel
            in
            ( newModel, Cmd.none )


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
