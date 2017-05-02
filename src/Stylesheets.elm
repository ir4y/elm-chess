port module Stylesheets exposing (..)

import Css.File exposing (..)
import ChessCss
import Html exposing (div)
import Html


port files : CssFileStructure -> Cmd msg


cssFiles : CssFileStructure
cssFiles =
    toFileStructure
        [ ( "chess.css", compile [ ChessCss.css ] )
        ]


main : CssCompilerProgram
main =
    Platform.program
        { init = ( (), files cssFiles )
        , update = \_ _ -> ( (), Cmd.none )
        , subscriptions = \_ -> Sub.none
        }
