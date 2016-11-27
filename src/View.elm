module View exposing (..)

import Html exposing (..)
import Html.Attributes exposing (rel, href)
import ChessCss
import Figure
import Html.CssHelpers
import DnD
import ChessDnD exposing (draggable, droppable)
import Msg
import Debug
import Movements


{ id, class, classList } =
    Html.CssHelpers.withNamespace "chess"


drawFigure figure =
    div [ class [ ChessCss.Figure ] ] [ Html.text <| Figure.figureToString figure ]


view model =
    Html.div []
        [ Html.node "link" [ rel "stylesheet", href "chess.css" ] []
        , Html.div
            [ id ChessCss.Chess ]
            (((List.range 1 8)
                |> List.map
                    (\i ->
                        ((List.range 1 8)
                            |> List.map (\j -> draw model j i)
                        )
                    )
             )
                |> List.concat
            )
        , DnD.dragged
            model.draggable
            (\(Figure.FigureOnDeck figure _) -> drawFigure figure)
        ]


draw model i j =
    case Figure.rowToHorizontalPosition i of
        Just hp ->
            case Figure.columnToVerticalPosition j of
                Just vp ->
                    drawCell model (Figure.Position hp vp)

                Nothing ->
                    text ""

        Nothing ->
            text ""


drawCell model position =
    let
        content = case Figure.getFromDeck position model.deck of
            Just figure -> draggable (Figure.FigureOnDeck figure position) [] [ drawFigure figure ]
            Nothing -> text ""
    in
        droppable (Msg.Dropped position)
            []
            [ content ]
