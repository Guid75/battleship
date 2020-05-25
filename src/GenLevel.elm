module GenLevel exposing
    ( buildShuffleCommand
    , computeAvailableCells
    , computeBoatCellPositions
    , createBoatStartCouples
    , tryToPlace
    )

import Array
import Matrix exposing (Matrix)
import Random
import Random.List
import Types exposing (Boat, CellType(..), Direction(..), GridCoord, Model, Msg(..))


directionGenerator : Random.Generator Direction
directionGenerator =
    Random.uniform North [ East, South, West ]


shuffleCoords : GridCoord -> List GridCoord -> Random.Generator GridCoord
shuffleCoords coord coords =
    Random.uniform coord coords


coordPlusDirection : Random.Generator GridCoord -> Random.Generator ( GridCoord, Direction )
coordPlusDirection coordGenerator =
    Random.pair coordGenerator directionGenerator


buildShuffleCommand : List GridCoord -> Cmd Msg
buildShuffleCommand cells =
    case cells of
        head :: tail ->
            let
                shuffleGen =
                    shuffleCoords head tail

                coordPlusDir =
                    coordPlusDirection shuffleGen
            in
            Random.generate GetCoordAndDirection coordPlusDir

        [] ->
            Cmd.none


createBoatStartCouples : Int -> Int -> Int -> Int -> List GridCoord
createBoatStartCouples minCol minRow maxCol maxRow =
    let
        matrix =
            Matrix.generate (maxCol - minCol + 1) (maxRow - minRow + 1) (\col row -> { col = col + minCol, row = row + minRow })
    in
    matrix
        |> Matrix.toArray
        |> Array.toList


computeBoatCellPositions : Boat -> List GridCoord
computeBoatCellPositions { pos, size, dir } =
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


isBoatColliding : Boat -> Matrix CellType -> Bool
isBoatColliding boat matrix =
    let
        boatCellPositions =
            computeBoatCellPositions boat

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
    List.any isCellColliding boatCellPositions


writeBoat : Boat -> Matrix CellType -> Matrix CellType
writeBoat boat gameMatrix =
    let
        boatCellPositions =
            computeBoatCellPositions boat

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

        writeBoatCell { col, row } matrix =
            Matrix.set col row Occupied matrix
                |> writeSurroundedCells ( col, row )
    in
    List.foldl writeBoatCell gameMatrix boatCellPositions


nextDir : Direction -> Direction
nextDir dir =
    case dir of
        North ->
            East

        East ->
            South

        South ->
            West

        West ->
            North


tryToPlace : Boat -> Matrix CellType -> ( Matrix CellType, Maybe Boat )
tryToPlace boat matrix =
    let
        dir2 =
            boat.dir |> nextDir

        dir3 =
            boat.dir |> nextDir |> nextDir

        dir4 =
            boat.dir |> nextDir |> nextDir |> nextDir

        boat2 =
            { boat | dir = dir2 }

        boat3 =
            { boat | dir = dir3 }

        boat4 =
            { boat | dir = dir4 }
    in
    if not (isBoatColliding boat matrix) then
        ( writeBoat boat matrix, Just boat )

    else if not (isBoatColliding boat2 matrix) then
        ( writeBoat boat2 matrix, Just boat2 )

    else if not (isBoatColliding boat3 matrix) then
        ( writeBoat boat3 matrix, Just boat3 )

    else if not (isBoatColliding boat4 matrix) then
        ( writeBoat boat4 matrix, Just boat4 )

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
