module Types exposing
    ( Direction(..)
    , FloatCoord
    , Grid
    , GridCoord
    , GridSize
    , Rect
    , Ship
    , ShipDef
    )


type alias ShipDef =
    { id : String
    , size : Int
    }


type alias Ship =
    { pos : GridCoord
    , size : Int
    , dir : Direction
    , id : String
    }


type Direction
    = North
    | East
    | South
    | West


type alias GridCoord =
    { col : Int
    , row : Int
    }


type alias GridSize =
    { width : Int
    , height : Int
    }


type alias FloatCoord =
    { x : Float
    , y : Float
    }


type alias Rect =
    { topLeft : GridCoord
    , bottomRight : GridCoord
    }


type alias Grid =
    { colCount : Int
    , rowCount : Int
    , boldInterval : Int
    , thinThickness : Float
    , boldThickness : Float
    , cellSize : Float
    , topLeft : FloatCoord
    , strokeColor : String
    }
