module View exposing (game)

import Array exposing (Array)
import Element exposing (..)
import Element.Background
import Element.Border
import Element.Font
import Game
import Html exposing (Html)
import Msg exposing (KeyEvent(..), Msg(..))


game : Game.Model -> List (Html Msg)
game model =
    [ layout [ Element.Font.size 60 ] <|
        column [ height fill, width fill ]
            [ score_ model.score
            , board_ model.board
            ]
    ]


mainColor : Color
mainColor =
    rgb 1 0.7 0.5


score_ : Int -> Element Msg
score_ _ =
    -- score
    --     |> String.fromInt
    -- |> (++) "Score: "
    text "Welcome :)"
        |> Element.el [ centerY, centerX ]
        |> List.singleton
        |> row [ width fill, height <| fillPortion 1, paddingXY 5 5, Element.Font.bold, Element.Font.color mainColor ]


board_ : Game.Board -> Element Msg
board_ board =
    board
        |> Array.map boardRow_
        |> Array.toList
        |> column [ centerX, centerY ]
        |> Element.el [ width fill, height <| fillPortion 5 ]


boardRow_ : Array Game.Cell -> Element Msg
boardRow_ cells =
    cells
        |> Array.map cell_
        |> Array.toList
        |> row [ spacing 20, paddingXY 0 10 ]


cell_ : Game.Cell -> Element Msg
cell_ cell =
    let
        value : String
        value =
            cell
                |> Maybe.map String.fromInt
                |> Maybe.withDefault "  "
    in
    column
        [ padding 60
        , Element.Border.rounded 10
        , Element.Font.semiBold
        , Element.Background.color mainColor
        ]
        [ text value ]
