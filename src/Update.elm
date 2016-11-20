module Update exposing (..)

import Msg exposing (Msg)
import Model exposing (Model)
import Figure
import DnD


update : Msg -> Model -> ( Model, Cmd.Cmd Msg )
update msg model =
    ( update_ msg model, Cmd.none )


update_ : Msg -> Model -> Model
update_ msg model =
    case msg of
        Msg.Dropped newPosition (Figure.FigureOnDeck figure oldPosition) ->
            { model
                | deck =
                    (model.deck
                        |> Figure.removeFromDeck oldPosition
                        |> Figure.insertToDeck newPosition figure
                    )
            }

        Msg.DnDMsg msg ->
            { model | draggable = DnD.update msg model.draggable }
