module ChessCss exposing (CssIds(..), CssClass(..), css)

import Css exposing (..)
import Css.Elements exposing (div)
import Css.Namespace exposing (namespace)


type CssIds
    = Chess


type CssClass
    = Figure
    | OverDrop


black =
    hex "#c0c0c0"


backgroundColorBlack =
    backgroundColor black


blackPositions =
    [ "16n+2"
    , "16n+4"
    , "16n+6"
    , "16n+8"
    , "16n+9"
    , "16n+11"
    , "16n+13"
    , "16n+15"
    ]


css =
    (stylesheet << namespace "chess")
        [ (#) Chess
            [ descendants
                [ div
                    ((blackPositions
                        |> List.map (\position -> nthOfType position [ backgroundColorBlack ])
                     )
                        ++ [ width (px 40)
                           , height (px 40)
                           , float left
                           , border3 (px 1) solid black
                           , nthOfType "8n+1" [ property "clear" "left" ]
                           ]
                    )
                ]
            ]
        , (.) Figure
            [ fontSize (px 30)
            , cursor pointer
            ]
        , (.) OverDrop
            [ backgroundColor (hex "e3ff00") |> important ]
        ]
