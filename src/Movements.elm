module Movements exposing (..)

import Figure exposing (incV, incH, decV, decH)


extractvalidDropCell : Figure.Deck -> Figure.FigureOnDeck -> List Figure.Position
extractvalidDropCell deck (Figure.FigureOnDeck (Figure.Figure color type_) position) =
    (case type_ of
        Figure.Pawn ->
            pawnValidDropCell deck color position

        Figure.Knight ->
            knightValidDropCell deck color position

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
                incV
            else
                decV

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
                |> Maybe.andThen incH
                |> Maybe.andThen (onlyIfFilledByColor deck enemyColor)

        strikeRight =
            oneSquareStep
                |> Maybe.andThen decH
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


knightValidDropCell : Figure.Deck -> Figure.FixureColor -> Figure.Position -> List (Maybe Figure.Position)
knightValidDropCell deck color position =
    let
        incV2 =
            Maybe.andThen incV << incV

        incH2 =
            Maybe.andThen incH << incH

        decV2 =
            Maybe.andThen decV << decV

        decH2 =
            Maybe.andThen decH << decH

        checkCellValid =
            nothingIfFilledByColor deck color

        doStep =
            (\inc1 inc2 -> position |> inc1 |> Maybe.andThen inc2 |> Maybe.andThen checkCellValid)
    in
        [ doStep incV2 decH
        , doStep incV2 incH
        , doStep decV2 decH
        , doStep decV2 incH
        , doStep incH2 decV
        , doStep incH2 incV
        , doStep decH2 decV
        , doStep decH2 incV
        ]


notingIfFilled : Figure.Deck -> Figure.Position -> Maybe Figure.Position
notingIfFilled deck position =
    if Figure.getFromDeck position deck == Nothing then
        Just position
    else
        Nothing


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


nothingIfFilledByColor : Figure.Deck -> Figure.FixureColor -> Figure.Position -> Maybe Figure.Position
nothingIfFilledByColor deck color position =
    case Figure.getFromDeck position deck of
        Just (Figure.Figure color_ _) ->
            if color_ == color then
                Nothing
            else
                Just position

        Nothing ->
            Just position
