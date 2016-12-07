module Main exposing (..)

import Html exposing (Html)
import Model exposing (init)
import Update exposing (update)
import View exposing (view)
import ChessDnD exposing (subscriptions)


main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions << .draggable
        }
