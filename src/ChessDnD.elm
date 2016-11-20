module ChessDnD exposing (..)

import DnD
import Msg


{ model, subscriptions, draggable, droppable } =
    DnD.init Msg.DnDMsg
