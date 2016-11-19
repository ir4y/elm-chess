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


position horizontalPosition verticalPositon =
    ( horizontalPositionToInt horizontalPosition, verticalPositonToInt verticalPositon )


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
getFromDeck (Position horizontalPosition verticalPositon) deck =
    Dict.get (position horizontalPosition verticalPositon) deck


initDeck : Deck
initDeck =
    Dict.fromList
        [ ( position A One, Figure White Rook )
        , ( position A Two, Figure White Knight )
        , ( position A Three, Figure White Bishop )
        , ( position A Four, Figure White Queen )
        , ( position A Five, Figure White King )
        , ( position A Six, Figure White Bishop )
        , ( position A Seven, Figure White Knight )
        , ( position A Eight, Figure White Rook )
        , ( position B One, Figure White Pawn )
        , ( position B Two, Figure White Pawn )
        , ( position B Three, Figure White Pawn )
        , ( position B Four, Figure White Pawn )
        , ( position B Five, Figure White Pawn )
        , ( position B Six, Figure White Pawn )
        , ( position B Seven, Figure White Pawn )
        , ( position B Eight, Figure White Pawn )
        , ( position H One, Figure Black Rook )
        , ( position H Two, Figure Black Knight )
        , ( position H Three, Figure Black Bishop )
        , ( position H Four, Figure Black Queen )
        , ( position H Five, Figure Black King )
        , ( position H Six, Figure Black Bishop )
        , ( position H Seven, Figure Black Knight )
        , ( position H Eight, Figure Black Rook )
        , ( position G One, Figure Black Pawn )
        , ( position G Two, Figure Black Pawn )
        , ( position G Three, Figure Black Pawn )
        , ( position G Four, Figure Black Pawn )
        , ( position G Five, Figure Black Pawn )
        , ( position G Six, Figure Black Pawn )
        , ( position G Seven, Figure Black Pawn )
        , ( position G Eight, Figure Black Pawn )
        ]


verticalPositonToInt verticalPositon =
    case verticalPositon of
        One ->
            1

        Two ->
            2

        Three ->
            3

        Four ->
            4

        Five ->
            5

        Six ->
            6

        Seven ->
            7

        Eight ->
            8


columnToVerticalPosition column =
    case column of
        1 ->
            Just One

        2 ->
            Just Two

        3 ->
            Just Three

        4 ->
            Just Four

        5 ->
            Just Five

        6 ->
            Just Six

        7 ->
            Just Seven

        8 ->
            Just Eight

        _ ->
            Nothing


rowToHorizontalPosition row =
    case row of
        8 ->
            Just A

        7 ->
            Just B

        6 ->
            Just C

        5 ->
            Just D

        4 ->
            Just E

        3 ->
            Just F

        2 ->
            Just G

        1 ->
            Just H

        _ ->
            Nothing


horizontalPositionToInt horizontalPosition =
    case horizontalPosition of
        A ->
            8

        B ->
            7

        C ->
            6

        D ->
            5

        E ->
            4

        F ->
            3

        G ->
            2

        H ->
            1
