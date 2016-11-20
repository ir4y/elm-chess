module View exposing (..)

import Html exposing (..)
import Html.Attributes exposing (rel, href)
import ChessCss
import Figure
import Html.CssHelpers
import DnD
import ChessDnD exposing (draggable, droppable)
import Msg


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
                            |> List.map (\j -> draw model i j)
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
    Figure.rowToHorizontalPosition i
        |> Maybe.andThen
            (\horizontalPosition ->
                (Figure.columnToVerticalPosition j
                    |> Maybe.map (Figure.Position horizontalPosition)
                )
            )
        |> Maybe.map (drawCell model)
        |> Maybe.withDefault (Html.text "")


drawCell model position =
    model.draggable
        |> DnD.getMeta
        |> Maybe.andThen
            (\(Figure.FigureOnDeck figure position_) ->
                if position == position_ then
                    Just (Html.div [] [])
                else
                    Nothing
            )
        |> Maybe.withDefault
            (case Figure.getFromDeck position model.deck of
                Just figure ->
                    draggable (Figure.FigureOnDeck figure position) [] [ drawFigure figure ]

                Nothing ->
                    let
                        isMouseOver =
                            case DnD.atDroppable model.draggable of
                                Just (Msg.Dropped position_ _) ->
                                    position_ == position

                                _ ->
                                    False
                    in
                        droppable (Msg.Dropped position)
                            [ classList [ ( ChessCss.OverDrop, isMouseOver ) ] ]
                            []
            )
