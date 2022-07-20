module Msg exposing (KeyEvent(..), Msg(..), keyToDirection)

import Game exposing (Cell, Direction(..), Position)


type Msg
    = KeyPressed KeyEvent
    | RandomCellGenerated ( Maybe Position, Cell )


type KeyEvent
    = KeyEventArrowUp
    | KeyEventArrowDown
    | KeyEventArrowLeft
    | KeyEventArrowRight
    | KeyEventUnknown


keyToDirection : KeyEvent -> Maybe Direction
keyToDirection event =
    case event of
        KeyEventArrowUp ->
            Just Up

        KeyEventArrowDown ->
            Just Down

        KeyEventArrowLeft ->
            Just Left

        KeyEventArrowRight ->
            Just Right

        KeyEventUnknown ->
            Nothing
