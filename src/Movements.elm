module Movements exposing (..)

import Figure


extractvalidDropCell : Figure.Deck -> Figure.FigureOnDeck -> List Figure.Position
extractvalidDropCell deck (Figure.FigureOnDeck (Figure.Figure color type_) position) =
    (case type_ of
        Figure.Pawn ->
            pawnValidDropCell deck color position

        _ ->
            []
    )
        |> List.foldr
            (\maybePosition acc ->
                case maybePosition of
                    Just position ->
                        position :: acc

                    Nothing ->
                        acc
            )
            []


pawnValidDropCell : Figure.Deck -> Figure.FixureColor -> Figure.Position -> List (Maybe Figure.Position)
pawnValidDropCell deck color position =
    let
        doStep =
            if color == Figure.Black then
                Figure.incV
            else
                Figure.decV

        firstStep =
            (doStep position)
                |> Maybe.andThen (notingIfFilled deck)
    in
        [ firstStep
        , firstStep
            |> Maybe.andThen doStep
            |> Maybe.andThen (notingIfFilled deck)
        ]


notingIfFilled : Figure.Deck -> Figure.Position -> Maybe Figure.Position
notingIfFilled deck position =
    if Figure.getFromDeck position deck == Nothing then
        Just position
    else
        Nothing
