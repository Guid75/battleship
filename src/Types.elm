module Types exposing
    ( Boat
    , CellType(..)
    , Direction(..)
    , FloatCoord
    , GridCoord
    , GridSize
    , Model
    , Msg(..)
    )

import Matrix exposing (Matrix)


type alias Model =
    { board : Matrix CellType
    , availableCells : List ( Int, Int )
    , boatsToPlace : List Int
    , placedBoats : List Boat
    }


type Msg
    = GetCouplePlusDirection ( ( Int, Int ), Direction )
    | Generate


type alias Boat =
    { pos : ( Int, Int )
    , size : Int
    , dir : Direction
    }


type CellType
    = Occupied
    | NextTo
    | Free


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
