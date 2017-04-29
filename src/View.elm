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
            (List.range 1 8
                |> List.concatMap
                    (\i ->
                        List.range 1 8
                            |> List.map (\j -> draw model j i)
                    )
            )
        , DnD.dragged
            model.draggable
            (\(Figure.FigureOnDeck figure _) -> drawFigure figure)
        ]


draw model i j =
    Maybe.map2
        (\hp vp -> drawCell model (Figure.Position hp vp))
        (Figure.rowToHorizontalPosition i)
        (Figure.columnToVerticalPosition j)
        |> Maybe.withDefault (Html.text "")


drawCell model position =
    let
        draggableFigure =
            DnD.getDragMeta model.draggable

        isCurrentDragging =
            draggableFigure
                |> Maybe.map
                    (\(Figure.FigureOnDeck figure position_) ->
                        position == position_
                    )
                |> Maybe.withDefault False

        validDropCells =
            draggableFigure
                |> Maybe.map (Movements.extractvalidDropCell model.deck)
                |> Maybe.withDefault []

        validToDrop =
            (validDropCells
                |> List.filter ((==) position)
                |> List.length
            )
                > 0

        isMouseOver =
            case DnD.getDropMeta model.draggable of
                Just position_ ->
                    position_ == position

                _ ->
                    False

        content =
            Figure.getFromDeck position model.deck
                |> Maybe.map
                    (\figure ->
                        draggable (Figure.FigureOnDeck figure position)
                            []
                            [ drawFigure figure ]
                    )
                |> Maybe.withDefault (text "")
    in
        if validToDrop then
            droppable position
                [ classList
                    [ ( ChessCss.ValidToDrop, not isMouseOver )
                    , ( ChessCss.OverDrop, isMouseOver )
                    ]
                ]
                [ content ]
        else
            div [ classList [ ( ChessCss.Dragging, isCurrentDragging ) ] ]
                [ content ]
