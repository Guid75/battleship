module GenLevel exposing
    ( buildShuffleCommand
    , computeAvailableCells
    , createBoatStartCouples
    , tryToPlace
    )

import Array
import Matrix exposing (Matrix)
import Random
import Random.List
import Types exposing (Boat, CellType(..), Direction(..), Model, Msg(..))


directionGenerator : Random.Generator Direction
directionGenerator =
    Random.uniform North [ East, South, West ]


shuffleCouples : ( Int, Int ) -> List ( Int, Int ) -> Random.Generator ( Int, Int )
shuffleCouples cell cells =
    Random.uniform cell cells


couplePlusDirection : Random.Generator ( Int, Int ) -> Random.Generator ( ( Int, Int ), Direction )
couplePlusDirection coupleGenerator =
    Random.pair coupleGenerator directionGenerator


buildShuffleCommand : List ( Int, Int ) -> Cmd Msg
buildShuffleCommand cells =
    case cells of
        head :: tail ->
            let
                shuffleGen =
                    shuffleCouples head tail

                couplePlusDir =
                    couplePlusDirection shuffleGen
            in
            Random.generate GetCouplePlusDirection couplePlusDir

        [] ->
            Cmd.none


createBoatStartCouples : Int -> Int -> Int -> Int -> List ( Int, Int )
createBoatStartCouples minX minY maxX maxY =
    let
        matrix =
            Matrix.generate (maxX - minX + 1) (maxY - minY + 1) (\x y -> ( x + minX, y + minY ))
    in
    matrix
        |> Matrix.toArray
        |> Array.toList


computeBoatCellPositions : Boat -> List ( Int, Int )
computeBoatCellPositions { pos, size, dir } =
    let
        ( xInit, yInit ) =
            pos
    in
    case dir of
        North ->
            List.foldl
                (\y positions -> ( xInit, y ) :: positions)
                []
            <|
                List.range (yInit - size + 1) yInit

        South ->
            List.foldl
                (\y positions -> ( xInit, y ) :: positions)
                []
            <|
                List.range yInit (yInit + size - 1)

        East ->
            List.foldl
                (\x positions -> ( x, yInit ) :: positions)
                []
            <|
                List.range xInit (xInit + size - 1)

        West ->
            List.foldl
                (\x positions -> ( x, yInit ) :: positions)
                []
            <|
                List.range (xInit - size + 1) xInit


isBoatColliding : Boat -> Matrix CellType -> Bool
isBoatColliding boat matrix =
    let
        boatCellPositions =
            computeBoatCellPositions boat

        isCellColliding ( x, y ) =
            if x < 0 || x > 9 || y < 0 || y > 10 then
                True

            else
                case Matrix.get x y matrix of
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

        writeSurroundedCellIfEmpty ( x, y ) matrix =
            case Matrix.get x y matrix of
                Ok Free ->
                    Matrix.set x y NextTo matrix

                _ ->
                    matrix

        writeSurroundedCells ( x, y ) matrix =
            matrix
                |> writeSurroundedCellIfEmpty ( x - 1, y )
                |> writeSurroundedCellIfEmpty ( x - 1, y - 1 )
                |> writeSurroundedCellIfEmpty ( x, y - 1 )
                |> writeSurroundedCellIfEmpty ( x + 1, y - 1 )
                |> writeSurroundedCellIfEmpty ( x + 1, y )
                |> writeSurroundedCellIfEmpty ( x + 1, y + 1 )
                |> writeSurroundedCellIfEmpty ( x, y + 1 )
                |> writeSurroundedCellIfEmpty ( x - 1, y + 1 )

        writeBoatCell ( x, y ) matrix =
            Matrix.set x y Occupied matrix
                |> writeSurroundedCells ( x, y )
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


computeAvailableCells : Matrix CellType -> List ( Int, Int )
computeAvailableCells matrix =
    matrix
        |> Matrix.indexedMap
            (\x y cellType ->
                if cellType == Free then
                    Just ( x, y )

                else
                    Nothing
            )
        |> Matrix.foldr
            (\v list ->
                case v of
                    Just ( x, y ) ->
                        ( x, y ) :: list

                    Nothing ->
                        list
            )
            []
