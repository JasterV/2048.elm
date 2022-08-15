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


setBoard : Board -> Model -> Model
setBoard board model =
    { model | board = board }


addScore : Int -> Model -> Model
addScore score model =
    { model | score = model.score + score }


foldUp_ : Model -> Model
foldUp_ model =
    let
        result =
            model.board
                |> Array.toList
                >> List.map Array.toList
                >> List.Extra.transpose
                >> List.map foldRowLeft_
                >> (\rows -> ( rows |> List.map Tuple.first, rows |> List.map Tuple.second ))
                >> Tuple.mapBoth
                    (List.foldl (+) 0)
                    (List.Extra.transpose
                        >> List.map Array.fromList
                        >> Array.fromList
                    )

        score =
            Tuple.first result

        newBoard =
            Tuple.second result
    in
    model
        |> setBoard newBoard
        |> addScore score


foldDown_ : Model -> Model
foldDown_ model =
    let
        result =
            model.board
                |> Array.toList
                >> List.map Array.toList
                >> List.Extra.transpose
                >> List.map foldRowRight_
                >> (\rows -> ( rows |> List.map Tuple.first, rows |> List.map Tuple.second ))
                >> Tuple.mapBoth
                    (List.foldl (+) 0)
                    (List.Extra.transpose
                        >> List.map Array.fromList
                        >> Array.fromList
                    )

        score =
            Tuple.first result

        newBoard =
            Tuple.second result
    in
    model
        |> setBoard newBoard
        |> addScore score


foldLeft_ : Model -> Model
foldLeft_ model =
    let
        result =
            model.board |> Array.map (Array.toList >> foldRowLeft_ >> Tuple.mapSecond Array.fromList)

        points =
            result |> Array.map Tuple.first |> Array.foldl (+) 0

        newBoard =
            Array.map Tuple.second result
    in
    model
        |> setBoard newBoard
        |> addScore points


foldRight_ : Model -> Model
foldRight_ model =
    let
        result =
            model.board |> Array.map (Array.toList >> foldRowRight_ >> Tuple.mapSecond Array.fromList)

        points =
            result |> Array.map Tuple.first |> Array.foldl (+) 0

        newBoard =
            Array.map Tuple.second result
    in
    model
        |> setBoard newBoard
        |> addScore points


foldRowLeft_ : List Cell -> ( Int, List Cell )
foldRowLeft_ =
    List.reverse >> foldRowRight_ >> Tuple.mapSecond List.reverse


foldRowRight_ : List Cell -> ( Int, List Cell )
foldRowRight_ row =
    row
        |> List.filter ((/=) Nothing)
        |> List.map (Maybe.withDefault 0)
        |> foldNumsListRight_
        |> Tuple.mapSecond
            (List.map Just
                >> (\folded -> List.repeat (List.length row - List.length folded) Nothing ++ folded)
            )


foldNumsListRight_ : List Int -> ( Int, List Int )
foldNumsListRight_ =
    doFoldNumsListRight_ 0 [ 0 ]


doFoldNumsListRight_ : Int -> List Int -> List Int -> ( Int, List Int )
doFoldNumsListRight_ score result original =
    case original of
        [] ->
            ( score, List.filter ((/=) 0) result )

        _ ->
            case ( List.Extra.last original, List.head result ) of
                ( Just last, Just first ) ->
                    if last == first then
                        doFoldNumsListRight_ (score + last + last) ([ 0, last + last ] ++ (List.tail result |> Maybe.withDefault [])) (pop_ original)

                    else
                        doFoldNumsListRight_ score (last :: result) (pop_ original)

                _ ->
                    ( score, [] )


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
