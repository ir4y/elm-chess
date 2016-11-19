module Main exposing (main)

import Html exposing (Html)
import Html.Attributes exposing (rel, href)
import ChessCss
import Html.CssHelpers


{ id, class, classList } =
    Html.CssHelpers.withNamespace "chess"


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


drawFigure pair =
    Html.div [ class [ ChessCss.Figure ] ] [ Html.text (figureAsString pair) ]


figureAsString pair =
    case pair of
        ( White, King ) ->
            String.fromChar '♔'

        ( White, Queen ) ->
            String.fromChar '♕'

        ( White, Rook ) ->
            String.fromChar '♖'

        ( White, Bishop ) ->
            String.fromChar '♗'

        ( White, Knight ) ->
            String.fromChar '♘'

        ( White, Pawn ) ->
            String.fromChar '♙'

        ( Black, King ) ->
            String.fromChar '♚'

        ( Black, Queen ) ->
            String.fromChar '♛'

        ( Black, Rook ) ->
            String.fromChar '♜'

        ( Black, Bishop ) ->
            String.fromChar '♝'

        ( Black, Knight ) ->
            String.fromChar '♞'

        ( Black, Pawn ) ->
            String.fromChar '♟'


deck =
    [ ( 1, 1, ( Black, Rook ) )
    , ( 2, 1, ( Black, Knight ) )
    , ( 3, 1, ( Black, Bishop ) )
    , ( 4, 1, ( Black, Queen ) )
    , ( 5, 1, ( Black, King ) )
    , ( 6, 1, ( Black, Bishop ) )
    , ( 7, 1, ( Black, Knight ) )
    , ( 8, 1, ( Black, Rook ) )
    , ( 1, 2, ( Black, Pawn ) )
    , ( 2, 2, ( Black, Pawn ) )
    , ( 3, 2, ( Black, Pawn ) )
    , ( 4, 2, ( Black, Pawn ) )
    , ( 5, 2, ( Black, Pawn ) )
    , ( 6, 2, ( Black, Pawn ) )
    , ( 7, 2, ( Black, Pawn ) )
    , ( 8, 2, ( Black, Pawn ) )
    , ( 1, 8, ( White, Rook ) )
    , ( 2, 8, ( White, Knight ) )
    , ( 3, 8, ( White, Bishop ) )
    , ( 4, 8, ( White, Queen ) )
    , ( 5, 8, ( White, King ) )
    , ( 6, 8, ( White, Bishop ) )
    , ( 7, 8, ( White, Knight ) )
    , ( 8, 8, ( White, Rook ) )
    , ( 1, 7, ( White, Pawn ) )
    , ( 2, 7, ( White, Pawn ) )
    , ( 3, 7, ( White, Pawn ) )
    , ( 4, 7, ( White, Pawn ) )
    , ( 5, 7, ( White, Pawn ) )
    , ( 6, 7, ( White, Pawn ) )
    , ( 7, 7, ( White, Pawn ) )
    , ( 8, 7, ( White, Pawn ) )
    ]


draw x y =
    Html.div []
        [ deck
            |> List.filter (\( y_, x_, pair ) -> x_ == x && y_ == y)
            |> List.head
            |> Maybe.map (\( _, _, pair ) -> drawFigure pair)
            |> Maybe.withDefault (Html.text "")
        ]


main : Html msg
main =
    Html.div []
        [ Html.node "link" [ rel "stylesheet", href "chess.css" ] []
        , Html.div
            [ id ChessCss.Chess ]
            (((List.range 1 8)
                |> List.map
                    (\i ->
                        ((List.range 1 8)
                            |> List.map (\j -> draw i j)
                        )
                    )
             )
                |> List.concat
            )
        ]
