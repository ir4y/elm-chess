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



--(>>=) = flip Maybe.andThen
--(<$>) = Maybe.map


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
    let
        draggableFigure =
            DnD.getMeta model.draggable

        isCurentDragging =
            draggableFigure
                |> Maybe.map (\(Figure.FigureOnDeck figure position_) -> position == position_)
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

        content =
            Figure.getFromDeck position model.deck
                |> Maybe.map (\figure -> draggable (Figure.FigureOnDeck figure position) [] [ drawFigure figure ])
                |> Maybe.withDefault (text "")

        isMouseOver =
            case DnD.atDroppable model.draggable of
                Just (Msg.Dropped position_ _) ->
                    position_ == position

                _ ->
                    False
    in
        if validToDrop then
            droppable (Msg.Dropped position)
                [ classList
                    [ ( ChessCss.ValidToDrop, not isMouseOver )
                    , ( ChessCss.OverDrop, isMouseOver )
                    ]
                ]
                [ content ]
        else
            div [ classList [ ( ChessCss.Dragging, isCurentDragging ) ] ]
                [ content ]
