module Grid exposing
    ( computeGridHeight
    , computeGridWidth
    , drawGrid
    , getCellCoord
    , getClosestCell
    , getGridTopLeft
    , isInGrid
    )

import Color
import Html
import Svg exposing (Svg, g, line, rect)
import Svg.Attributes exposing (cx, cy, fill, fillOpacity, height, id, r, stroke, strokeWidth, width, x, x1, x2, y, y1, y2)
import Svg.Events
import Types exposing (FloatCoord, Grid, GridCoord, GridSize, Msg(..), Rect)


isInGrid : FloatCoord -> Grid -> Bool
isInGrid coord grid =
    (coord.x >= grid.topLeft.x)
        && (coord.x < grid.topLeft.x + computeGridWidth grid)
        && (coord.y >= grid.topLeft.y)
        && (coord.y < grid.topLeft.y + computeGridHeight grid)


getThicknessByIndex : Grid -> Int -> Float
getThicknessByIndex grid index =
    if modBy grid.boldInterval index == 0 then
        grid.boldThickness

    else
        grid.thinThickness


drawVerticalLine : Grid -> Int -> List (Svg msg) -> List (Svg msg)
drawVerticalLine grid colIndex lines =
    let
        x =
            getNthLineOffset grid colIndex
    in
    line
        [ x1 <| String.fromFloat <| x + grid.topLeft.x
        , y1 <| String.fromFloat grid.topLeft.y
        , x2 <| String.fromFloat <| x + grid.topLeft.x
        , y2 <| String.fromFloat <| computeGridHeight grid + grid.topLeft.y
        , stroke grid.strokeColor
        , strokeWidth <| String.fromFloat <| getThicknessByIndex grid colIndex
        ]
        []
        :: lines


drawHorizontalLine : Grid -> Int -> List (Svg msg) -> List (Svg msg)
drawHorizontalLine grid rowIndex lines =
    let
        y =
            getNthLineOffset grid rowIndex
    in
    line
        [ x1 <| String.fromFloat grid.topLeft.x
        , y1 <| String.fromFloat <| y + grid.topLeft.y
        , x2 <| String.fromFloat <| computeGridWidth grid + grid.topLeft.x
        , y2 <| String.fromFloat <| y + grid.topLeft.y
        , stroke grid.strokeColor
        , strokeWidth <| String.fromFloat <| getThicknessByIndex grid rowIndex
        ]
        []
        :: lines


getNthLineOffset : Grid -> Int -> Float
getNthLineOffset grid lineNumber =
    let
        dec =
            (lineNumber + grid.boldInterval - 1) // grid.boldInterval

        thickness =
            getThicknessByIndex grid lineNumber
    in
    toFloat lineNumber * grid.cellSize + toFloat (lineNumber - dec) * grid.thinThickness + toFloat dec * grid.boldThickness + thickness / 2.0


computeGridWidth : Grid -> Float
computeGridWidth grid =
    let
        lastLineIndex =
            grid.colCount

        offset =
            getNthLineOffset grid lastLineIndex

        thickness =
            getThicknessByIndex grid lastLineIndex
    in
    offset + thickness / 2.0


computeGridHeight : Grid -> Float
computeGridHeight grid =
    let
        lastLineIndex =
            grid.rowCount

        offset =
            getNthLineOffset grid lastLineIndex

        thickness =
            getThicknessByIndex grid lastLineIndex
    in
    offset + thickness / 2.0


getGridTopLeft : Grid -> FloatCoord
getGridTopLeft grid =
    let
        offset =
            getNthLineOffset grid 0
    in
    { x = offset
    , y = offset
    }


drawHorizontalNumeration : Int -> String -> Grid -> Svg msg
drawHorizontalNumeration column str grid =
    let
        topLeftCellCoord =
            getCellCoord column 0 grid
    in
    Svg.text_
        [ x <| String.fromFloat <| topLeftCellCoord.x + grid.cellSize / 2.0
        , y <| String.fromFloat <| grid.topLeft.y - 3.0
        , Svg.Attributes.color "black"
        , Svg.Attributes.fontSize "20px"
        , Svg.Attributes.textAnchor "middle"
        ]
        [ Svg.text str ]


drawHorizontalNumerations : Grid -> List (Svg msg)
drawHorizontalNumerations grid =
    List.range 1 grid.colCount
        |> List.map (\index -> drawHorizontalNumeration (index - 1) (String.fromInt index) grid)


drawVerticalNumeration : Int -> String -> Grid -> Svg msg
drawVerticalNumeration row str grid =
    let
        topLeftCellCoord =
            getCellCoord 0 row grid
    in
    Svg.text_
        [ x <| String.fromFloat <| grid.topLeft.x - 3.0
        , y <| String.fromFloat <| topLeftCellCoord.y + grid.cellSize / 2.0
        , Svg.Attributes.color "black"
        , Svg.Attributes.fontSize "20px"
        , Svg.Attributes.textAnchor "end"
        , Svg.Attributes.dominantBaseline "middle"
        ]
        [ Svg.text str ]


drawVerticalNumerations : Grid -> List (Svg msg)
drawVerticalNumerations grid =
    List.range 1 grid.rowCount
        |> List.map (\index -> drawVerticalNumeration (index - 1) (index + 64 |> Char.fromCode |> String.fromChar) grid)


drawGrid : Grid -> List (Html.Attribute msg) -> Svg msg
drawGrid grid animAttrs =
    g
        animAttrs
        (List.concat
            [ List.foldl (drawVerticalLine grid) [] (List.range 0 grid.colCount)
            , List.foldl (drawHorizontalLine grid) [] (List.range 0 grid.rowCount)
            , drawHorizontalNumerations grid
            , drawVerticalNumerations grid
            ]
        )


getCellCoord : Int -> Int -> Grid -> FloatCoord
getCellCoord col row grid =
    let
        colThickness =
            getThicknessByIndex grid col

        rowThickness =
            getThicknessByIndex grid row
    in
    { x = getNthLineOffset grid col + colThickness / 2.0 + grid.topLeft.x
    , y = getNthLineOffset grid row + rowThickness / 2.0 + grid.topLeft.y
    }


getClosestRow : Float -> Grid -> Int
getClosestRow y grid =
    let
        getRow : Int -> Int
        getRow row =
            let
                upperHalfThickness =
                    getThicknessByIndex grid (row + 1) / 2.0

                upperCoord =
                    getCellCoord 0 (row + 1) grid
            in
            if row >= grid.rowCount then
                grid.rowCount - 1

            else if y < upperCoord.y - upperHalfThickness then
                row

            else
                getRow <| row + 1
    in
    getRow 0


getClosestCol : Float -> Grid -> Int
getClosestCol x grid =
    let
        getCol : Int -> Int
        getCol col =
            let
                rightHalfThickness =
                    getThicknessByIndex grid (col + 1) / 2.0

                rightCoord =
                    getCellCoord (col + 1) 0 grid
            in
            if col >= grid.colCount then
                grid.colCount - 1

            else if x < rightCoord.x - rightHalfThickness then
                col

            else
                getCol <| col + 1
    in
    getCol 0


getClosestCell : FloatCoord -> Grid -> GridCoord
getClosestCell { x, y } grid =
    { col = getClosestCol x grid
    , row = getClosestRow y grid
    }
