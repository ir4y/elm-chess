module ChessCss exposing (CssIds(..), CssClass(..), css)

import Css exposing (..)
import Css.Elements exposing (div)
import Css.Namespace exposing (namespace)


type CssIds
    = Chess


type CssClass
    = Figure
    | OverDrop
    | ValidToDrop
    | Dragging


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
        [ div [ fontSize (px 30) ]
        , id Chess
            [ descendants
                [ div
                    ((blackPositions
                        |> List.map
                            (\position ->
                                nthOfType position
                                    [ backgroundColorBlack
                                    ]
                            )
                     )
                        ++ [ width (px 40)
                           , height (px 40)
                           , float left
                           , border3 (px 1) solid black
                           , nthOfType "8n+1" [ property "clear" "left" ]
                           ]
                    )
                , class Figure
                    [ cursor pointer
                    , width (px 30)
                    , height (px 30)
                    , border (px 0)
                    , margin2 (px 3) (px 5)
                    ]
                , class OverDrop
                    [ backgroundColor (hex "e3ff00") |> important ]
                , class ValidToDrop
                    [ backgroundColor (hex "48f925") |> important ]
                , class Dragging
                    [ descendants [ class Figure [ opacity (num 0.3) ] ] ]
                ]
            ]
        ]
