module Update exposing (..)

import Msg exposing (Msg)
import Model exposing (Model)
import Figure
import DnD


update : Msg -> Model -> ( Model, Cmd.Cmd Msg )
update msg model =
    ( update_ msg model, Cmd.none )


inCaseOfCastlingMoveRook : Figure.Figure -> Figure.Position -> Figure.Deck -> Figure.Deck
inCaseOfCastlingMoveRook (Figure.Figure color type_) (Figure.Position hp vp) deck =
    if type_ == Figure.King && hp == Figure.B || hp == Figure.G then
        let
            rookPosition =
                Figure.Position
                    (if hp == Figure.B then
                        Figure.A
                     else
                        Figure.H
                    )
                    vp

            rookDestination =
                Figure.Position
                    (if hp == Figure.B then
                        Figure.D
                     else
                        Figure.F
                    )
                    vp

            maybeRook =
                Figure.getFromDeck rookPosition deck
        in
            case maybeRook of
                Just rook ->
                    deck
                        |> Figure.removeFromDeck rookPosition
                        |> Figure.insertToDeck rookDestination rook

                Nothing ->
                    deck
    else
        deck


update_ : Msg -> Model -> Model
update_ msg model =
    case msg of
        Msg.Dropped newPosition (Figure.FigureOnDeck figure oldPosition) ->
            { model
                | deck =
                    (model.deck
                        |> Figure.removeFromDeck oldPosition
                        |> Figure.insertToDeck newPosition figure
                        |> inCaseOfCastlingMoveRook figure newPosition
                    )
            }

        Msg.DnDMsg msg ->
            { model | draggable = DnD.update msg model.draggable }
