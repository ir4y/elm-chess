module Msg exposing (..)

import Figure
import DnD


type Msg
    = Dropped Figure.Position Figure.FigureOnDeck
    | DnDMsg (DnD.Msg Figure.Position Figure.FigureOnDeck)
