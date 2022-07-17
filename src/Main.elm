module Main exposing (..)

import Browser
import Element as UI
import Game
import Html exposing (Html)
import Msg exposing (Msg)


type alias Document msg =
    { title : String
    , body : List (Html msg)
    }


main : Program () Game.Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }


init : () -> ( Game.Model, Cmd Msg )
init _ =
    ( Game.initialModel
    , Cmd.none
    )


update : Msg -> Game.Model -> ( Game.Model, Cmd Msg )
update msg model =
    case msg of
        Msg.ArrowDown ->
            ( model |> Game.moveDown
            , Cmd.none
            )

        Msg.ArrowUp ->
            ( model |> Game.moveUp
            , Cmd.none
            )

        Msg.ArrowLeft ->
            ( model |> Game.moveLeft
            , Cmd.none
            )

        Msg.ArrowRight ->
            ( model |> Game.moveRight
            , Cmd.none
            )


view : Game.Model -> Document Msg
view model =
    { title = "2048"
    , body = view_ model
    }


view_ : Game.Model -> List (Html Msg)
view_ model =
    [ UI.layout [] <|
        UI.column []
            [ viewScore_ model.score
            , viewBoard_ model.board
            ]
    ]


viewScore_ : Int -> UI.Element Msg
viewScore_ score =
    score
        |> String.fromInt
        |> UI.text
        |> List.singleton
        |> UI.row []


viewBoard_ : Game.Board -> UI.Element Msg
viewBoard_ board =
    board
        |> List.map viewBoardRow_
        |> UI.column []


viewBoardRow_ : List Game.Cell -> UI.Element Msg
viewBoardRow_ cells =
    cells
        |> List.map viewCell_
        |> UI.row []


viewCell_ : Game.Cell -> UI.Element Msg
viewCell_ cell =
    let
        value : String
        value =
            cell
                |> Maybe.map String.fromInt
                |> Maybe.withDefault "-"
    in
    UI.column []
        [ UI.text value ]
