module Model exposing (..)

import DnD
import Figure
import Msg exposing (Msg)
import ChessDnD exposing (model)


type alias Model =
    { draggable : DnD.Draggable Figure.FigureOnDeck Msg
    , deck : Figure.Deck
    }


init : ( Model, Cmd Msg )
init =
    ( { draggable = model
      , deck = Figure.initDeck
      }
    , Cmd.none
    )
