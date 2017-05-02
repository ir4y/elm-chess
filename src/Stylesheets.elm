port module Stylesheets exposing (..)

import Css.File exposing (..)
import ChessCss
import Html exposing (div)
import Html


port files : CssFileStructure -> Cmd msg


cssFiles : CssFileStructure
cssFiles =
    toFileStructure
        [ ( "src/chess.css", compile [ ChessCss.css ] )
        ]


main : CssCompilerProgram
main =
    Css.File.compiler files cssFiles
