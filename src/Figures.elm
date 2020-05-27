module Figures exposing
    ( drawRect
    , drawTarget
    )

import Color
import Grid
import Svg exposing (Svg, g, line, rect)
import Svg.Attributes exposing (cx, cy, fill, fillOpacity, height, id, r, stroke, strokeWidth, width, x, x1, x2, y, y1, y2)
import Svg.Events
import Types exposing (FloatCoord, Grid, GridCoord, GridSize, Msg(..))


drawTarget : Grid -> Color.Color -> GridCoord -> Float -> Svg Msg
drawTarget grid color { col, row } fireAmount =
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
            , fill <| Color.toCssString color
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


drawRect : List (Svg.Attribute Msg) -> Grid -> String -> Color.Color -> Float -> GridCoord -> GridSize -> Svg Msg
drawRect attrs grid id_ color padding topLeft size =
    let
        topLeftCellCoord =
            Grid.getCellCoord topLeft.col topLeft.row grid

        bottomRight =
            Grid.getCellCoord (topLeft.col + size.width - 1) (topLeft.row + size.height - 1) grid

        bottomRightCoord =
            { x = bottomRight.x + grid.cellSize, y = bottomRight.y + grid.cellSize }

        innerAttrs =
            [ id id_
            , x <| String.fromFloat <| topLeftCellCoord.x + padding
            , y <| String.fromFloat <| topLeftCellCoord.y + padding
            , width <| String.fromFloat <| bottomRightCoord.x - topLeftCellCoord.x - padding * 2.0
            , height <| String.fromFloat <| bottomRightCoord.y - topLeftCellCoord.y - padding * 2.0
            , fill <| Color.toCssString color
            , Svg.Events.onMouseOver <| PieceOver id_
            , Svg.Events.onMouseOut <| PieceOut id_
            ]
    in
    rect
        (List.concat
            [ innerAttrs
            , attrs
            ]
        )
        []
