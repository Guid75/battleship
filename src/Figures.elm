module Figures exposing
    ( computeGridSizeByDirection
    , drawHit
    , drawMiss
    , drawShip
    , drawShipFloating
    , drawShipPlacement
    , drawTarget
    , sizeToColor
    )

import Color
import Grid
import Model exposing (Msg(..))
import Svg exposing (Svg, g, line, rect)
import Svg.Attributes exposing (cx, cy, fill, fillOpacity, height, id, r, rx, ry, stroke, strokeWidth, width, x, x1, x2, y, y1, y2)
import Svg.Events
import Types exposing (Direction(..), FloatCoord, Grid, GridCoord, GridSize, Ship)


drawTarget : Grid -> GridCoord -> Float -> Svg msg
drawTarget grid { col, row } fireAmount =
    let
        cellCoord =
            Grid.getCellCoord col row grid
    in
    Svg.g
        []
        [ Svg.circle
            [ cx <| String.fromFloat <| cellCoord.x + grid.cellSize / 2.0
            , cy <| String.fromFloat <| cellCoord.y + grid.cellSize / 2.0
            , r <| String.fromFloat <| grid.cellSize / 2.0 - 2.0
            , fill <| Color.toCssString Color.gray
            , stroke "#FF0000"
            , strokeWidth "2.0"
            ]
            []
        , Svg.circle
            [ cx <| String.fromFloat <| cellCoord.x + grid.cellSize / 2.0
            , cy <| String.fromFloat <| cellCoord.y + grid.cellSize / 2.0
            , r <| String.fromFloat <| (grid.cellSize / 2.0 - 2.0) * fireAmount
            , fill <| Color.toCssString Color.yellow
            , stroke "#FF0000"
            , strokeWidth "2.0"
            ]
            []
        ]


drawMiss : Grid -> GridCoord -> Svg msg
drawMiss grid { col, row } =
    let
        cellCoord =
            Grid.getCellCoord col row grid
    in
    Svg.circle
        [ cx <| String.fromFloat <| cellCoord.x + grid.cellSize / 2.0
        , cy <| String.fromFloat <| cellCoord.y + grid.cellSize / 2.0
        , r <| String.fromFloat <| grid.cellSize / 2.0 - 8.0
        , fill <| Color.toCssString Color.blue
        , strokeWidth "2.0"
        ]
        []


drawHit : Grid -> GridCoord -> Svg msg
drawHit grid { col, row } =
    let
        cellCoord =
            Grid.getCellCoord col row grid
    in
    Svg.circle
        [ cx <| String.fromFloat <| cellCoord.x + grid.cellSize / 2.0
        , cy <| String.fromFloat <| cellCoord.y + grid.cellSize / 2.0
        , r <| String.fromFloat <| grid.cellSize / 2.0 - 4.0
        , fill <| Color.toCssString Color.red
        , strokeWidth "2.0"
        ]
        []


computeGridSizeByDirection : Ship -> GridSize
computeGridSizeByDirection { dir, size } =
    case dir of
        South ->
            { width = 1, height = size }

        East ->
            { width = size, height = 1 }

        North ->
            { width = 1, height = size }

        West ->
            { width = size, height = 1 }


computeTopLeftPosition : Ship -> GridCoord
computeTopLeftPosition { dir, pos, size } =
    case dir of
        South ->
            pos

        East ->
            pos

        North ->
            { col = pos.col, row = pos.row - size + 1 }

        West ->
            { col = pos.col - size + 1, row = pos.row }


sizeToColor : Int -> Color.Color
sizeToColor size =
    case size of
        2 ->
            Color.rgb255 200 136 136

        3 ->
            Color.rgb255 136 200 136

        4 ->
            Color.rgb255 136 136 255

        5 ->
            Color.rgb255 136 136 136

        _ ->
            Color.rgb255 136 136 136


drawShip : Ship -> Grid -> Bool -> Svg Msg
drawShip ship grid focused =
    let
        topLeft =
            computeTopLeftPosition ship

        topLeftCellCoord =
            Grid.getCellCoord topLeft.col topLeft.row grid

        size =
            computeGridSizeByDirection ship

        bottomRight =
            Grid.getCellCoord (topLeft.col + size.width - 1) (topLeft.row + size.height - 1) grid

        bottomRightCoord =
            { x = bottomRight.x + grid.cellSize, y = bottomRight.y + grid.cellSize }

        padding =
            2.0

        color =
            sizeToColor ship.size

        strokeC =
            Color.darkGray

        strokeW =
            if focused then
                3.0

            else
                1.0
    in
    rect
        [ id ship.id
        , rx "10.0"
        , ry "10.0"
        , x <| String.fromFloat <| topLeftCellCoord.x + padding
        , y <| String.fromFloat <| topLeftCellCoord.y + padding
        , width <| String.fromFloat <| bottomRightCoord.x - topLeftCellCoord.x - padding * 2.0
        , height <| String.fromFloat <| bottomRightCoord.y - topLeftCellCoord.y - padding * 2.0
        , fill <| Color.toCssString color
        , stroke <| Color.toCssString strokeC
        , strokeWidth <| String.fromFloat <| strokeW
        , Svg.Events.onMouseOver <| PieceOver ship.id
        , Svg.Events.onMouseOut <| PieceOut ship.id
        ]
        []


drawShipFloating : Ship -> Grid -> FloatCoord -> Svg msg
drawShipFloating ship grid topLeftCellCoord =
    let
        size =
            computeGridSizeByDirection ship

        fakeTopLeftCellCoord =
            Grid.getCellCoord 0 0 grid

        fakeBottomRight =
            Grid.getCellCoord (size.width - 1) (size.height - 1) grid

        fakeBottomRightCoord =
            { x = fakeBottomRight.x + grid.cellSize, y = fakeBottomRight.y + grid.cellSize }

        offset =
            { x = fakeTopLeftCellCoord.x - topLeftCellCoord.x
            , y = fakeTopLeftCellCoord.y - topLeftCellCoord.y
            }

        bottomRightCoord =
            { x = fakeBottomRightCoord.x - offset.x
            , y = fakeBottomRightCoord.y - offset.y
            }

        padding =
            2.0

        color =
            let
                rgba =
                    Color.toRgba <| sizeToColor ship.size

                withAlpha =
                    { rgba | alpha = 0.7 }
            in
            Color.fromRgba withAlpha
    in
    rect
        [ id ship.id
        , rx "10.0"
        , ry "10.0"
        , x <| String.fromFloat <| topLeftCellCoord.x + padding
        , y <| String.fromFloat <| topLeftCellCoord.y + padding
        , width <| String.fromFloat <| bottomRightCoord.x - topLeftCellCoord.x - padding * 2.0
        , height <| String.fromFloat <| bottomRightCoord.y - topLeftCellCoord.y - padding * 2.0
        , fill <| Color.toCssString color
        , stroke "gray"
        ]
        []


drawShipPlacement : Ship -> Grid -> Svg msg
drawShipPlacement ship grid =
    let
        topLeft =
            computeTopLeftPosition ship

        size =
            computeGridSizeByDirection ship

        topLeftCellCoord =
            Grid.getCellCoord topLeft.col topLeft.row grid

        bottomRight =
            Grid.getCellCoord (topLeft.col + size.width - 1) (topLeft.row + size.height - 1) grid

        bottomRightCoord =
            { x = bottomRight.x + grid.cellSize, y = bottomRight.y + grid.cellSize }
    in
    rect
        [ x <| String.fromFloat <| topLeftCellCoord.x
        , y <| String.fromFloat <| topLeftCellCoord.y
        , width <| String.fromFloat <| bottomRightCoord.x - topLeftCellCoord.x
        , height <| String.fromFloat <| bottomRightCoord.y - topLeftCellCoord.y
        , fill <| Color.toCssString <| Color.rgba 0.5 1 0.5 0.2
        , stroke <| Color.toCssString <| Color.rgb255 0 125 0
        , strokeWidth "2.0"
        ]
        []
