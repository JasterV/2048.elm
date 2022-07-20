module Main exposing (..)

import Browser
import Browser.Events
import Cmd exposing (generateRandomPositionedCell)
import Element exposing (..)
import Game
import Html exposing (Html)
import Json.Decode as Decode
import Msg exposing (KeyEvent(..), Msg(..))
import View



-- MAIN


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
        , subscriptions = subscriptions
        }



-- INIT


init : () -> ( Game.Model, Cmd Msg )
init _ =
    let
        model =
            Game.initialModel
    in
    ( model
    , generateRandomPositionedCell model
    )



-- SUBSCRIPTIONS


subscriptions : Game.Model -> Sub Msg
subscriptions _ =
    Browser.Events.onKeyDown keyDecoder


keyDecoder : Decode.Decoder Msg
keyDecoder =
    Decode.map toMsg (Decode.field "key" Decode.string)


toMsg : String -> Msg
toMsg string =
    case string of
        "ArrowLeft" ->
            Msg.KeyPressed KeyEventArrowLeft

        "ArrowRight" ->
            Msg.KeyPressed KeyEventArrowRight

        "ArrowDown" ->
            Msg.KeyPressed KeyEventArrowDown

        "ArrowUp" ->
            Msg.KeyPressed KeyEventArrowUp

        _ ->
            Msg.KeyPressed KeyEventUnknown



-- UPDATE


update : Msg -> Game.Model -> ( Game.Model, Cmd Msg )
update msg model =
    case msg of
        Msg.KeyPressed KeyEventUnknown ->
            ( model, Cmd.none )

        Msg.KeyPressed key ->
            let
                newModel : Game.Model
                newModel =
                    key
                        |> Msg.keyToDirection
                        |> Maybe.map (Game.move model)
                        |> Maybe.withDefault model

                cmd =
                    if newModel == model then
                        Cmd.none

                    else
                        generateRandomPositionedCell newModel
            in
            ( newModel, cmd )

        Msg.RandomCellGenerated ( position, cell ) ->
            ( position
                |> Maybe.map (Game.addCell model cell)
                |> Maybe.withDefault model
            , Cmd.none
            )



-- VIEW


view : Game.Model -> Document Msg
view model =
    { title = "2048"
    , body = View.game model
    }
