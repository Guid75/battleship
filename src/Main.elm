module Main exposing (main)

import Animator
import Animator.Inline
import Array
import Browser
import GenLevel
import Grid exposing (Grid, drawGrid)
import Html exposing (Html, button, div, span, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Matrix exposing (Matrix)
import Random
import Random.List
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)
import Time
import Types exposing (Boat, CellType(..), Direction(..), Model, Msg(..))


boatsBySize =
    [ 5, 4, 3, 3, 2 ]


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


createFreshMatrix : Matrix CellType
createFreshMatrix =
    Matrix.repeat 10 10 Free


initModel : Model -> Model
initModel model =
    { model
        | board = createFreshMatrix
        , availableCells = GenLevel.createBoatStartCouples 0 0 9 9
        , boatsToPlace = boatsBySize
        , placedBoats = []
    }


init : () -> ( Model, Cmd Msg )
init flags =
    ( { board = createFreshMatrix
      , availableCells = GenLevel.createBoatStartCouples 0 0 9 9
      , boatsToPlace = boatsBySize
      , placedBoats = []
      }
    , Cmd.none
    )


displayRow : Matrix CellType -> Int -> Html msg
displayRow matrix rowIndex =
    let
        cellTypeToHtml cellType =
            case cellType of
                Occupied ->
                    span [] [ Html.text "O" ]

                NextTo ->
                    span [] [ Html.text "|" ]

                Free ->
                    span [] [ Html.text "." ]
    in
    case Matrix.getRow rowIndex matrix of
        Ok row ->
            div
                []
            <|
                Array.toList <|
                    Array.map (\cellType -> cellTypeToHtml cellType) row

        Err _ ->
            div
                []
                []


sizeToColor : Int -> String
sizeToColor size =
    case size of
        2 ->
            "#FF888888"

        3 ->
            "#88FF8888"

        4 ->
            "#8888FF88"

        5 ->
            "#88888888"

        _ ->
            "#88888888"


boatToSvg grid boat =
    let
        ( x, y ) =
            boat.pos

        color =
            sizeToColor boat.size
    in
    case boat.dir of
        South ->
            Grid.drawRect grid color 2 { col = x, row = y } { width = 1, height = boat.size }

        East ->
            Grid.drawRect grid color 2 { col = x, row = y } { width = boat.size, height = 1 }

        North ->
            Grid.drawRect grid color 2 { col = x, row = y - boat.size + 1 } { width = 1, height = boat.size }

        West ->
            Grid.drawRect grid color 2 { col = x - boat.size + 1, row = y } { width = boat.size, height = 1 }


generateBoats grid boats =
    List.map (boatToSvg grid) boats


viewGrid model =
    let
        grid =
            Grid 10 10 10 1 2 30 { x = 0, y = 0 } "#000000"

        svgBoats =
            generateBoats grid model.placedBoats
    in
    svg
        [ width "400"
        , height "400"
        , viewBox "0 0 400 400"
        ]
    <|
        List.concat
            [ [ drawGrid grid [] ]
            , svgBoats

            -- [ Grid.drawRect grid "#FF888888" 2 { col = 4, row = 3 } { width = 1, height = 4 }
            -- , Grid.drawRect grid "#88FF8888" 2 { col = 6, row = 1 } { width = 3, height = 1 }
            -- ]
            --        , Grid.drawSquare grid "#FF0000" 2 { col = 1, row = 1 }
            ]


view model =
    div
        []
        [ viewGrid model
        , button [ Html.Events.onClick Generate ] [ Html.text "Generate" ]
        ]


iterPlacement : ( Int, Int ) -> Direction -> Model -> ( Model, Cmd Msg )
iterPlacement pos dir model =
    case model.boatsToPlace of
        head :: tail ->
            case GenLevel.tryToPlace (Boat pos head dir) model.board of
                ( matrix, Just boat ) ->
                    let
                        newModel =
                            { model
                                | board = matrix
                                , availableCells = GenLevel.computeAvailableCells matrix
                                , boatsToPlace = tail
                                , placedBoats = boat :: model.placedBoats
                            }
                    in
                    ( newModel, GenLevel.buildShuffleCommand newModel.availableCells )

                _ ->
                    ( model, GenLevel.buildShuffleCommand model.availableCells )

        [] ->
            ( model, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case Debug.log "msg" msg of
        GetCouplePlusDirection ( pos, dir ) ->
            iterPlacement pos dir model

        Generate ->
            let
                newModel =
                    initModel model
            in
            ( newModel, GenLevel.buildShuffleCommand newModel.availableCells )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
