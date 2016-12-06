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

        (Figure.Position hp vp) =
            position

        isFirstMovement =
            case ( color, vp ) of
                ( Figure.White, Figure.Two ) ->
                    True

                ( Figure.Black, Figure.Seven ) ->
                    True

                _ ->
                    False

        oneSquareStep =
            (doStep position)

        twoSquareStep =
            oneSquareStep
                |> Maybe.andThen doStep

        strikeLeft =
            oneSquareStep
                |> Maybe.andThen Figure.incH
                |> Maybe.andThen (onlyIfFilledByColor deck enemyColor)

        strikeRight =
            oneSquareStep
                |> Maybe.andThen Figure.decH
                |> Maybe.andThen (onlyIfFilledByColor deck enemyColor)

        enemyColor =
            Figure.invertColor color
    in
        [ oneSquareStep
            |> Maybe.andThen (notingIfFilled deck)
        , if isFirstMovement then
            twoSquareStep
                |> Maybe.andThen (notingIfFilled deck)
          else
            Nothing
        , strikeLeft
        , strikeRight
        ]


notingIfFilled : Figure.Deck -> Figure.Position -> Maybe Figure.Position
notingIfFilled deck position =
    if Figure.getFromDeck position deck == Nothing then
        Just position
    else
        Nothing


notingIfFilledByColor : Figure.Deck -> Figure.FixureColor -> Figure.Position -> Maybe Figure.Position
notingIfFilledByColor deck color position =
onlyIfFilledByColor : Figure.Deck -> Figure.FixureColor -> Figure.Position -> Maybe Figure.Position
onlyIfFilledByColor deck color position =
    Figure.getFromDeck position deck
        |> Maybe.map (\(Figure.Figure color_ _) -> color_ == color)
        |> Maybe.andThen
            (\flag ->
                if flag then
                    Just position
                else
                    Nothing
            )
