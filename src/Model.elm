module Model exposing
    ( Board
    , Model
    , Msg(..)
    , State(..)
    , Turn(..)
    )

import Dict exposing (Dict)
import Html.Events.Extra.Mouse as Mouse
import Matrix exposing (Matrix)
import ShipGen
import Time
import Types exposing (Direction(..), FloatCoord, Grid, GridCoord, Ship)


type Turn
    = Player
    | CPU


type State
    = Preparing
    | Playing Turn
    | End Turn


type alias Board =
    { shipGenerator : ShipGen.Generator
    , ships : Dict String Ship
    , shots : Matrix Bool
    , cellUnderMouse : Maybe GridCoord
    , grid : Grid
    , id : String
    }


type alias Model =
    { myBoard : Board
    , cpuBoard : Board
    , currentMousePos : FloatCoord
    , clickedShip : Maybe Ship
    , clickedCell : Maybe GridCoord
    , clickedPos : FloatCoord
    , focusedShip : Maybe Ship
    , firingCell : Maybe GridCoord
    , draggingShip : Bool
    , state : State
    }


type Msg
    = GetCoordAndDirectionPlayer ( GridCoord, Direction )
    | GetCoordAndDirectionCPU ( GridCoord, Direction )
    | Generate Turn
    | PieceOver String
    | PieceOut String
    | Launch
    | MouseMove String Mouse.Event
    | MouseDown String Mouse.Event
    | MouseUp String Mouse.Event
    | SvgMousePosResult ( String, Float, Float )
    | GetCellCandidate GridCoord
    | Tick Time.Posix
    | PlayCPU
    | NewGame
