module Game exposing
    ( Board
    , Cell
    , Model
    , initialModel
    , isCellEmpty
    , moveDown
    , moveLeft
    , moveRight
    , moveUp
    )

-- TYPE DEFINITIONS


type alias Model =
    { score : Int
    , board : Board
    }


type alias Board =
    List (List Cell)


type alias Cell =
    Maybe Int


isCellEmpty : Cell -> Bool
isCellEmpty cell =
    case cell of
        Nothing ->
            True

        _ ->
            False



-- VALUES


boardSize_ : Int
boardSize_ =
    4


initialBoard_ : Board
initialBoard_ =
    List.repeat boardSize_
        (List.repeat boardSize_ Nothing)


initialModel : Model
initialModel =
    Model 0 initialBoard_



-- GAME RULES


generateRandomCell_ : Model -> Model
generateRandomCell_ model =
    model


foldDown_ : Model -> Model
foldDown_ model =
    model


foldUp_ : Model -> Model
foldUp_ model =
    model


foldLeft_ : Model -> Model
foldLeft_ model =
    model


foldRight_ : Model -> Model
foldRight_ model =
    model


moveDown : Model -> Model
moveDown model =
    model
        |> foldDown_
        |> generateRandomCell_


moveUp : Model -> Model
moveUp model =
    model
        |> foldUp_
        |> generateRandomCell_


moveLeft : Model -> Model
moveLeft model =
    model
        |> foldLeft_
        |> generateRandomCell_


moveRight : Model -> Model
moveRight model =
    model
        |> foldRight_
        |> generateRandomCell_
