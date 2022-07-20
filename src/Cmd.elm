module Cmd exposing (generateRandomPositionedCell)

import Array exposing (Array)
import Game exposing (Cell, Model, Position)
import Msg exposing (Msg)
import Random


pickRandomPosition : Array Position -> Random.Generator (Maybe Position)
pickRandomPosition positions =
    let
        head =
            positions |> Array.toList |> List.head

        tail =
            positions |> Array.toList |> List.tail |> Maybe.withDefault []
    in
    Random.uniform head (tail |> List.map Just)


generateRandomCell : Random.Generator Cell
generateRandomCell =
    Random.uniform (Just 2) ([ 2, 2, 2, 2, 2, 4 ] |> List.map Just)


generateRandomPositionedCell : Model -> Cmd Msg
generateRandomPositionedCell model =
    let
        positions =
            Game.getFreePositions model
    in
    Random.generate Msg.RandomCellGenerated (Random.pair (pickRandomPosition positions) generateRandomCell)
