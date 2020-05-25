module Types exposing
    ( Boat
    , BoatDef
    , CellType(..)
    , Direction(..)
    , FloatCoord
    , Grid
    , GridCoord
    , GridSize
    , Model
    , Msg(..)
    , Turn(..)
    )

import Dict exposing (Dict)
import Html.Events.Extra.Mouse as Mouse
import Matrix exposing (Matrix)


type Turn
    = Player
    | CPU


type alias Board =
    { matrix : Matrix CellType
    , boats : Dict String Boat
    }


type alias Model =
    { myBoard : Board
    , cpuBoard : Board
    , boatsToPlace : List BoatDef
    , turn : Maybe Turn
    , myGrid : Grid
    , cpuGrid : Grid
    , currentMousePos : ( Float, Float )
    , clickedBoat : Maybe Boat
    , clickedCell : Maybe GridCoord
    }


type Msg
    = GetCoordAndDirection ( GridCoord, Direction )
    | Generate Turn
    | PieceOver
    | PieceOut
    | PieceDown String
    | PieceUp String
    | MouseMove String Mouse.Event
    | SvgMousePosResult ( String, Float, Float )


type alias BoatDef =
    { id : String
    , size : Int
    }


type alias Boat =
    { pos : GridCoord
    , size : Int
    , dir : Direction
    , id : String
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
