module Types exposing
    ( Board
    , Boat
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

import Animator
import Animator.Inline
import Dict exposing (Dict)
import Html.Events.Extra.Mouse as Mouse
import Matrix exposing (Matrix)
import Time


type Turn
    = Player
    | CPU


type alias Board =
    { matrix : Matrix CellType
    , boatsToPlace : List BoatDef
    , boats : Dict String Boat
    , shots : List GridCoord
    , cellOver : Maybe GridCoord
    , grid : Grid
    , id : String
    }


type alias Model =
    { myBoard : Board
    , cpuBoard : Board
    , currentMousePos : FloatCoord
    , clickedBoat : Maybe Boat
    , clickedCell : Maybe GridCoord
    , clickedPos : FloatCoord
    , focusedBoat : Maybe Boat
    , focusedUp : Animator.Timeline Bool
    , firing : Animator.Timeline Bool
    , firingCell : Maybe GridCoord
    }


type Msg
    = GetCoordAndDirection Turn ( GridCoord, Direction )
    | Generate Turn
    | PieceOver String
    | PieceOut String
    | MouseMove String Mouse.Event
    | MouseDown String Mouse.Event
    | MouseUp String Mouse.Event
    | SvgMousePosResult ( String, Float, Float )
    | Blink
    | Tick Time.Posix


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
