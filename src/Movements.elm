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
        |> List.filter (\i -> i /= Nothing)
        |> (List.map <|
                Maybe.withDefault (Figure.Position Figure.A Figure.One)
           )


notingIfFilled : Figure.Deck -> Figure.Position -> Maybe Figure.Position
notingIfFilled deck position =
    if Figure.getFromDeck position deck == Nothing then
        Just position
    else
        Nothing


pawnValidDropCell : Figure.Deck -> Figure.FixureColor -> Figure.Position -> List (Maybe Figure.Position)
pawnValidDropCell deck color (Figure.Position h v) =
    let
        fn =
            if color == Figure.Black then
                Figure.incV
            else
                Figure.decV
    in
        [ (fn v)
            |> Maybe.map (Figure.Position h)
            |> Maybe.andThen (notingIfFilled deck)
        , (fn v)
            |> Maybe.andThen
                (\v ->
                    if Figure.getFromDeck (Figure.Position h v) deck == Nothing then
                        Just v
                    else
                        Nothing
                )
            |> Maybe.andThen (fn)
            |> Maybe.map (Figure.Position h)
            |> Maybe.andThen (notingIfFilled deck)
        ]
