module Figures exposing
    ( drawBoat
    , drawBoatFloating
    , drawBoatPlacement
    , drawHit
    , drawMiss
    , drawTarget
    )

import Color
import Grid
import Svg exposing (Svg, g, line, rect)
import Svg.Attributes exposing (cx, cy, fill, fillOpacity, height, id, r, rx, ry, stroke, strokeWidth, width, x, x1, x2, y, y1, y2)
import Svg.Events
import Types exposing (Boat, Direction(..), FloatCoord, Grid, GridCoord, GridSize, Msg(..))


drawTarget : Grid -> GridCoord -> Float -> Svg Msg
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


drawMiss : Grid -> GridCoord -> Svg Msg
drawMiss grid { col, row } =
    let
        cellCoord =
            Grid.getCellCoord col row grid
    in
    Svg.circle
        [ cx <| String.fromFloat <| cellCoord.x + grid.cellSize / 2.0
        , cy <| String.fromFloat <| cellCoord.y + grid.cellSize / 2.0
        , r <| String.fromFloat <| grid.cellSize / 2.0 - 4.0
        , fill <| Color.toCssString Color.blue
        , strokeWidth "2.0"
        ]
        []


drawHit : Grid -> GridCoord -> Svg Msg
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


computeTopLeftPosition : Boat -> GridCoord
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
            Color.rgb255 255 136 136

        3 ->
            Color.rgb255 136 255 136

        4 ->
            Color.rgb255 136 136 255

        5 ->
            Color.rgb255 136 136 136

        _ ->
            Color.rgb255 136 136 136


drawBoat : Boat -> Grid -> List (Svg.Attribute Msg) -> Svg Msg
drawBoat boat grid attrs =
    let
        topLeft =
            computeTopLeftPosition boat

        topLeftCellCoord =
            Grid.getCellCoord topLeft.col topLeft.row grid

        size =
            computeGridSizeByDirection boat.dir boat.size

        bottomRight =
            Grid.getCellCoord (topLeft.col + size.width - 1) (topLeft.row + size.height - 1) grid

        bottomRightCoord =
            { x = bottomRight.x + grid.cellSize, y = bottomRight.y + grid.cellSize }

        padding =
            2.0

        color =
            sizeToColor boat.size

        innerAttrs =
            [ id boat.id
            , rx "10.0"
            , ry "10.0"
            , x <| String.fromFloat <| topLeftCellCoord.x + padding
            , y <| String.fromFloat <| topLeftCellCoord.y + padding
            , width <| String.fromFloat <| bottomRightCoord.x - topLeftCellCoord.x - padding * 2.0
            , height <| String.fromFloat <| bottomRightCoord.y - topLeftCellCoord.y - padding * 2.0
            , fill <| Color.toCssString color
            , stroke "gray"
            , Svg.Events.onMouseOver <| PieceOver boat.id
            , Svg.Events.onMouseOut <| PieceOut boat.id
            ]
    in
    rect
        (List.concat
            [ innerAttrs
            , attrs
            ]
        )
        []


drawBoatFloating : Boat -> Grid -> FloatCoord -> Svg Msg
drawBoatFloating boat grid topLeftCellCoord =
    let
        size =
            computeGridSizeByDirection boat.dir boat.size

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
            sizeToColor boat.size

        innerAttrs =
            [ id boat.id
            , rx "10.0"
            , ry "10.0"
            , x <| String.fromFloat <| topLeftCellCoord.x + padding
            , y <| String.fromFloat <| topLeftCellCoord.y + padding
            , width <| String.fromFloat <| bottomRightCoord.x - topLeftCellCoord.x - padding * 2.0
            , height <| String.fromFloat <| bottomRightCoord.y - topLeftCellCoord.y - padding * 2.0
            , fill <| Color.toCssString color
            , stroke "gray"
            ]
    in
    rect
        innerAttrs
        []


drawBoatPlacement : Boat -> Grid -> Svg Msg
drawBoatPlacement boat grid =
    let
        topLeft =
            computeTopLeftPosition boat

        size =
            computeGridSizeByDirection boat.dir boat.size

        topLeftCellCoord =
            Grid.getCellCoord topLeft.col topLeft.row grid

        bottomRight =
            Grid.getCellCoord (topLeft.col + size.width - 1) (topLeft.row + size.height - 1) grid

        bottomRightCoord =
            { x = bottomRight.x + grid.cellSize, y = bottomRight.y + grid.cellSize }

        innerAttrs =
            [ x <| String.fromFloat <| topLeftCellCoord.x
            , y <| String.fromFloat <| topLeftCellCoord.y
            , width <| String.fromFloat <| bottomRightCoord.x - topLeftCellCoord.x
            , height <| String.fromFloat <| bottomRightCoord.y - topLeftCellCoord.y
            , fill <| Color.toCssString <| Color.rgba 0.5 0.5 1 0.2
            , stroke <| Color.toCssString <| Color.rgb255 50 50 255
            ]
    in
    rect
        (List.concat
            [ innerAttrs
            ]
        )
        []
