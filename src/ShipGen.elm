module ShipGen exposing
    ( Generator
    , computeShipCellPositions
    , createGenerator
    , feed
    , getProducedShips
    , run
    )

import Array
import Dict exposing (Dict)
import Matrix exposing (Matrix)
import Random
import Random.List
import Types exposing (Direction(..), GridCoord, GridSize, Ship, ShipDef)


type CellType
    = Occupied
    | NextTo
    | Free


type Generator
    = Generator
        { matrix : Matrix CellType
        , shipDefsToPlace : List ShipDef
        , shipsProduced : Dict String Ship
        }


getProducedShips : Generator -> Dict String Ship
getProducedShips generator =
    case generator of
        Generator { shipsProduced } ->
            shipsProduced


createGenerator : GridSize -> List ShipDef -> Generator
createGenerator gridSize shipDefs =
    let
        matrix =
            Matrix.repeat gridSize.width gridSize.height Free
    in
    Generator
        { matrix = matrix
        , shipDefsToPlace = shipDefs
        , shipsProduced = Dict.empty
        }


run : Generator -> (( GridCoord, Direction ) -> msg) -> Cmd msg
run generator msg =
    case generator of
        Generator { matrix } ->
            case computeAvailableCells matrix of
                head :: tail ->
                    let
                        shuffleGen =
                            shuffleCoords head tail

                        coordPlusDir =
                            coordPlusDirection shuffleGen
                    in
                    Random.generate msg coordPlusDir

                [] ->
                    Cmd.none


feed : GridCoord -> Direction -> Generator -> (( GridCoord, Direction ) -> msg) -> ( Generator, Cmd msg )
feed pos dir generator msg =
    case generator of
        Generator { matrix, shipDefsToPlace, shipsProduced } ->
            case shipDefsToPlace of
                head :: tail ->
                    case tryToPlace (Ship pos head.size dir head.id) matrix of
                        ( newMatrix, Just ship ) ->
                            let
                                newGenerator =
                                    Generator
                                        { matrix = newMatrix
                                        , shipDefsToPlace = tail
                                        , shipsProduced = Dict.insert head.id ship shipsProduced
                                        }
                            in
                            ( newGenerator
                            , run newGenerator msg
                            )

                        _ ->
                            ( generator
                            , run generator msg
                            )

                [] ->
                    ( generator, Cmd.none )


directionGenerator : Random.Generator Direction
directionGenerator =
    Random.uniform North [ East, South, West ]


shuffleCoords : GridCoord -> List GridCoord -> Random.Generator GridCoord
shuffleCoords coord coords =
    Random.uniform coord coords


coordPlusDirection : Random.Generator GridCoord -> Random.Generator ( GridCoord, Direction )
coordPlusDirection coordGenerator =
    Random.pair coordGenerator directionGenerator


computeShipCellPositions : Ship -> List GridCoord
computeShipCellPositions { pos, size, dir } =
    case dir of
        North ->
            List.foldl
                (\row positions -> { col = pos.col, row = row } :: positions)
                []
            <|
                List.range (pos.row - size + 1) pos.row

        South ->
            List.foldl
                (\row positions -> { col = pos.col, row = row } :: positions)
                []
            <|
                List.range pos.row (pos.row + size - 1)

        East ->
            List.foldl
                (\col positions -> { col = col, row = pos.row } :: positions)
                []
            <|
                List.range pos.col (pos.col + size - 1)

        West ->
            List.foldl
                (\col positions -> { col = col, row = pos.row } :: positions)
                []
            <|
                List.range (pos.col - size + 1) pos.col


isShipColliding : Ship -> Matrix CellType -> Bool
isShipColliding ship matrix =
    let
        shipCellPositions =
            computeShipCellPositions ship

        isCellColliding { col, row } =
            if col < 0 || col > 9 || row < 0 || row > 10 then
                True

            else
                case Matrix.get col row matrix of
                    Ok value ->
                        value /= Free

                    Err _ ->
                        True
    in
    List.any isCellColliding shipCellPositions


writeShip : Ship -> Matrix CellType -> Matrix CellType
writeShip ship gameMatrix =
    let
        shipCellPositions =
            computeShipCellPositions ship

        writeSurroundedCellIfEmpty ( col, row ) matrix =
            case Matrix.get col row matrix of
                Ok Free ->
                    Matrix.set col row NextTo matrix

                _ ->
                    matrix

        writeSurroundedCells ( col, row ) matrix =
            matrix
                |> writeSurroundedCellIfEmpty ( col - 1, row )
                |> writeSurroundedCellIfEmpty ( col - 1, row - 1 )
                |> writeSurroundedCellIfEmpty ( col, row - 1 )
                |> writeSurroundedCellIfEmpty ( col + 1, row - 1 )
                |> writeSurroundedCellIfEmpty ( col + 1, row )
                |> writeSurroundedCellIfEmpty ( col + 1, row + 1 )
                |> writeSurroundedCellIfEmpty ( col, row + 1 )
                |> writeSurroundedCellIfEmpty ( col - 1, row + 1 )

        writeShipCell { col, row } matrix =
            Matrix.set col row Occupied matrix
                |> writeSurroundedCells ( col, row )
    in
    List.foldl writeShipCell gameMatrix shipCellPositions


rotateDir : Direction -> Direction
rotateDir dir =
    case dir of
        North ->
            East

        East ->
            South

        South ->
            West

        West ->
            North


tryToPlace : Ship -> Matrix CellType -> ( Matrix CellType, Maybe Ship )
tryToPlace ship matrix =
    let
        dir2 =
            ship.dir |> rotateDir

        dir3 =
            ship.dir |> rotateDir |> rotateDir

        dir4 =
            ship.dir |> rotateDir |> rotateDir |> rotateDir

        ship2 =
            { ship | dir = dir2 }

        ship3 =
            { ship | dir = dir3 }

        ship4 =
            { ship | dir = dir4 }
    in
    if not (isShipColliding ship matrix) then
        ( writeShip ship matrix, Just ship )

    else if not (isShipColliding ship2 matrix) then
        ( writeShip ship2 matrix, Just ship2 )

    else if not (isShipColliding ship3 matrix) then
        ( writeShip ship3 matrix, Just ship3 )

    else if not (isShipColliding ship4 matrix) then
        ( writeShip ship4 matrix, Just ship4 )

    else
        ( matrix, Nothing )


computeAvailableCells : Matrix CellType -> List GridCoord
computeAvailableCells matrix =
    matrix
        |> Matrix.indexedMap
            (\col row cellType ->
                if cellType == Free then
                    Just { col = col, row = row }

                else
                    Nothing
            )
        |> Matrix.foldr
            (\v list ->
                case v of
                    Just { col, row } ->
                        { col = col, row = row } :: list

                    Nothing ->
                        list
            )
            []
