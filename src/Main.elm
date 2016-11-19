module Main exposing (..)

import Html exposing (Html)
import Html.Attributes exposing (rel, href)
import Figure
import View
import DnD
import ChessCss
import Html.CssHelpers


{ id, class, classList } =
    Html.CssHelpers.withNamespace "chess"



-- Model


type alias Model =
    { draggable : DnD.Draggable Figure.FigureOnDeck Msg
    , deck : Figure.Deck
    }


type Msg
    = Dropped Figure.Position Figure.FigureOnDeck
    | DnDMsg (DnD.Msg Figure.FigureOnDeck Msg)


dnd =
    DnD.init DnDMsg


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ dnd.subscriptions model.draggable
        ]


init : ( Model, Cmd Msg )
init =
    ( Model dnd.model Figure.initDeck, Cmd.none )


update : Msg -> Model -> ( Model, Cmd.Cmd Msg )
update msg model =
    ( update_ msg model, Cmd.none )


update_ : Msg -> Model -> Model
update_ msg model =
    case msg of
        Dropped newPosition figure ->
            model

        DnDMsg msg ->
            { model | draggable = DnD.update msg model.draggable }


main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


view model =
    Html.div []
        [ Html.node "link" [ rel "stylesheet", href "chess.css" ] []
        , Html.div
            [ id ChessCss.Chess ]
            (((List.range 1 8)
                |> List.map
                    (\i ->
                        ((List.range 1 8)
                            |> List.map (\j -> draw model i j)
                        )
                    )
             )
                |> List.concat
            )
        ]


draw model i j =
    Figure.rowToHorizontalPosition i
        |> Maybe.andThen
            (\horizontalPosition ->
                (Figure.columnToVerticalPosition j
                    |> Maybe.map (Figure.Position horizontalPosition)
                )
            )
        |> Maybe.map (View.drawCell model.deck)
        |> Maybe.withDefault (Html.text "")
