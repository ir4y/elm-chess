module View exposing (..)

import Html exposing (..)
import ChessCss
import Figure
import Html.CssHelpers


{ id, class, classList } =
    Html.CssHelpers.withNamespace "chess"


drawFigure figure =
    div [ class [ ChessCss.Figure ] ] [ Html.text <| Figure.figureToString figure ]


drawCell deck position =
    Html.div []
        [ Figure.getFromDeck position deck
            |> Maybe.map drawFigure
            |> Maybe.withDefault (Html.text "")
        ]
