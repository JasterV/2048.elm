module Game exposing
    ( Board
    , Cell
    , Direction(..)
    , Model
    , Position
    , addCell
    , getFreePositions
    , initialModel
    , move
    , setBoard
    )

import Array exposing (Array)
import List.Extra



-- TYPE DEFINITIONS


type alias Model =
    { score : Int
    , board : Board
    }


type alias Board =
    Array (Array Cell)


type alias Cell =
    Maybe Int


type alias Position =
    { row : Int
    , col : Int
    }


type Direction
    = Up
    | Down
    | Left
    | Right



-- VALUES


boardSize_ : Int
boardSize_ =
    4


initialBoard_ : Board
initialBoard_ =
    Array.repeat boardSize_
        (Array.repeat boardSize_ Nothing)


initialModel : Model
initialModel =
    Model 0 initialBoard_



-- GAME RULES


flatten_ : Array (Array a) -> Array a
flatten_ =
    Array.foldr Array.append Array.empty


pop_ : List a -> List a
pop_ l =
    List.Extra.removeAt (List.length l - 1) l


getFreePositions : Model -> Array Position
getFreePositions =
    .board
        >> flatten_
        >> Array.indexedMap
            (\index cell ->
                ( Position (index // boardSize_) (modBy boardSize_ index), cell )
            )
        >> Array.filter (Tuple.second >> (==) Maybe.Nothing)
        >> Array.map Tuple.first


addCell : Model -> Cell -> Position -> Model
addCell model cell position =
    {--
  Adds a cell on the given position.
  If the position is already full, no action is taken 
--}
    let
        board =
            model.board

        row =
            Array.get position.row board

        newRow =
            row
                |> Maybe.withDefault Array.empty
                |> Array.set position.col cell

        newBoard =
            Array.set position.row newRow board
    in
    { model | board = newBoard }


setBoard : Model -> Board -> Model
setBoard model board =
    { model | board = board }


foldUp_ : Model -> Model
foldUp_ model =
    let
        newBoard =
            model.board
                |> Array.toList
                >> List.map Array.toList
                >> List.Extra.transpose
                >> List.map foldRowLeft_
                >> List.Extra.transpose
                >> List.map Array.fromList
                >> Array.fromList
    in
    setBoard model newBoard


foldDown_ : Model -> Model
foldDown_ model =
    let
        newBoard =
            model.board
                |> Array.toList
                >> List.map Array.toList
                >> List.Extra.transpose
                >> List.map foldRowRight_
                >> List.Extra.transpose
                >> List.map Array.fromList
                >> Array.fromList
    in
    setBoard model newBoard


foldLeft_ : Model -> Model
foldLeft_ model =
    let
        newBoard =
            model.board |> Array.map (Array.toList >> foldRowLeft_ >> Array.fromList)
    in
    setBoard model newBoard


foldRight_ : Model -> Model
foldRight_ model =
    let
        newBoard =
            model.board |> Array.map (Array.toList >> foldRowRight_ >> Array.fromList)
    in
    setBoard model newBoard


foldRowLeft_ : List Cell -> List Cell
foldRowLeft_ =
    List.reverse >> foldRowRight_ >> List.reverse


foldRowRight_ : List Cell -> List Cell
foldRowRight_ row =
    row
        |> List.filter ((/=) Nothing)
        |> List.map (Maybe.withDefault 0)
        |> foldNumsListRight_
        |> List.map Just
        |> (\folded -> List.repeat (List.length row - List.length folded) Nothing ++ folded)


foldNumsListRight_ : List Int -> List Int
foldNumsListRight_ =
    doFoldNumsListRight_ [ 0 ]


doFoldNumsListRight_ : List Int -> List Int -> List Int
doFoldNumsListRight_ result original =
    case original of
        [] ->
            List.filter ((/=) 0) result

        _ ->
            case ( List.Extra.last original, List.head result ) of
                ( Just last, Just first ) ->
                    if last == first then
                        doFoldNumsListRight_ ([ 0, last + last ] ++ (List.tail result |> Maybe.withDefault [])) (pop_ original)

                    else
                        doFoldNumsListRight_ (last :: result) (pop_ original)

                _ ->
                    []


move : Model -> Direction -> Model
move model key =
    case key of
        Down ->
            model |> foldDown_

        Up ->
            model |> foldUp_

        Left ->
            model |> foldLeft_

        Right ->
            model |> foldRight_
