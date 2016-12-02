module Figure exposing (..)

import Dict exposing (Dict)


type HorizontalPosition
    = A
    | B
    | C
    | D
    | E
    | F
    | G
    | H


type VerticalPosition
    = One
    | Two
    | Three
    | Four
    | Five
    | Six
    | Seven
    | Eight


type Position
    = Position HorizontalPosition VerticalPosition


type FigureType
    = King
    | Queen
    | Rook
    | Bishop
    | Knight
    | Pawn


type FixureColor
    = Black
    | White


type Figure
    = Figure FixureColor FigureType


type FigureOnDeck
    = FigureOnDeck Figure Position


figureToString : Figure -> String
figureToString figure =
    case figure of
        Figure White King ->
            "♔"

        Figure White Queen ->
            "♕"

        Figure White Rook ->
            "♖"

        Figure White Bishop ->
            "♗"

        Figure White Knight ->
            "♘"

        Figure White Pawn ->
            "♙"

        Figure Black King ->
            "♚"

        Figure Black Queen ->
            "♛"

        Figure Black Rook ->
            "♜"

        Figure Black Bishop ->
            "♝"

        Figure Black Knight ->
            "♞"

        Figure Black Pawn ->
            "♟"


type alias Deck =
    Dict ( Int, Int ) Figure


getFromDeck : Position -> Deck -> Maybe Figure
getFromDeck (Position horizontalPosition verticalPosition) =
    Dict.get (position horizontalPosition verticalPosition)


insertToDeck : Position -> Figure -> Deck -> Deck
insertToDeck (Position horizontalPosition verticalPosition) =
    Dict.insert (position horizontalPosition verticalPosition)


removeFromDeck : Position -> Deck -> Deck
removeFromDeck (Position horizontalPosition verticalPosition) =
    Dict.remove (position horizontalPosition verticalPosition)


initDeck : Deck
initDeck =
    Dict.fromList
        [ ( position A One, Figure White Rook )
        , ( position B One, Figure White Knight )
        , ( position C One, Figure White Bishop )
        , ( position D One, Figure White Queen )
        , ( position E One, Figure White King )
        , ( position F One, Figure White Bishop )
        , ( position G One, Figure White Knight )
        , ( position H One, Figure White Rook )
        , ( position A Two, Figure White Pawn )
        , ( position B Two, Figure White Pawn )
        , ( position C Two, Figure White Pawn )
        , ( position D Two, Figure White Pawn )
        , ( position E Two, Figure White Pawn )
        , ( position F Two, Figure White Pawn )
        , ( position G Two, Figure White Pawn )
        , ( position H Two, Figure White Pawn )
        , ( position A Eight, Figure Black Rook )
        , ( position B Eight, Figure Black Knight )
        , ( position C Eight, Figure Black Bishop )
        , ( position D Eight, Figure Black Queen )
        , ( position E Eight, Figure Black King )
        , ( position F Eight, Figure Black Bishop )
        , ( position G Eight, Figure Black Knight )
        , ( position H Eight, Figure Black Rook )
        , ( position A Seven, Figure Black Pawn )
        , ( position B Seven, Figure Black Pawn )
        , ( position C Seven, Figure Black Pawn )
        , ( position D Seven, Figure Black Pawn )
        , ( position E Seven, Figure Black Pawn )
        , ( position F Seven, Figure Black Pawn )
        , ( position G Seven, Figure Black Pawn )
        , ( position H Seven, Figure Black Pawn )
        ]


position horizontalPosition verticalPosition =
    ( horizontalPositionToInt horizontalPosition, verticalPositionToInt verticalPosition )


verticalPositionToInt verticalPosition =
    case verticalPosition of
        One ->
            8

        Two ->
            7

        Three ->
            6

        Four ->
            5

        Five ->
            4

        Six ->
            3

        Seven ->
            2

        Eight ->
            1


horizontalPositionToInt horizontalPosition =
    case horizontalPosition of
        A ->
            1

        B ->
            2

        C ->
            3

        D ->
            4

        E ->
            5

        F ->
            6

        G ->
            7

        H ->
            8


columnToVerticalPosition column =
    case column of
        8 ->
            Just One

        7 ->
            Just Two

        6 ->
            Just Three

        5 ->
            Just Four

        4 ->
            Just Five

        3 ->
            Just Six

        2 ->
            Just Seven

        1 ->
            Just Eight

        _ ->
            Nothing


rowToHorizontalPosition row =
    case row of
        1 ->
            Just A

        2 ->
            Just B

        3 ->
            Just C

        4 ->
            Just D

        5 ->
            Just E

        6 ->
            Just F

        7 ->
            Just G

        8 ->
            Just H

        _ ->
            Nothing


incH : Position -> Maybe Position
incH (Position horizontalPosition verticalPosition) =
    horizontalPosition
        |> horizontalPositionToInt
        |> (+) 1
        |> rowToHorizontalPosition
        |> Maybe.map (\horizontalPosition -> Position horizontalPosition verticalPosition)


decH : Position -> Maybe Position
decH (Position horizontalPosition verticalPosition) =
    horizontalPosition
        |> horizontalPositionToInt
        |> (\row -> row - 1)
        |> rowToHorizontalPosition
        |> Maybe.map (\horizontalPosition -> Position horizontalPosition verticalPosition)


incV : Position -> Maybe Position
incV (Position horizontalPosition verticalPosition) =
    verticalPosition
        |> verticalPositionToInt
        |> (+) 1
        |> columnToVerticalPosition
        |> Maybe.map (Position horizontalPosition)


decV : Position -> Maybe Position
decV (Position horizontalPosition verticalPosition) =
    verticalPosition
        |> verticalPositionToInt
        |> (\column -> column - 1)
        |> columnToVerticalPosition
        |> Maybe.map (Position horizontalPosition)
