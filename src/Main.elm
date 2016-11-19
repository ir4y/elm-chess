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
        Dropped newPosition (Figure.FigureOnDeck figure oldPosition) ->
            { model
                | deck =
                    (model.deck
                        |> Figure.removeFromDeck oldPosition
                        |> Figure.insertToDeck newPosition figure
                    )
            }

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
        , DnD.dragged
            model.draggable
            (\(Figure.FigureOnDeck figure _) -> View.drawFigure figure)
        ]


draw model i j =
    Figure.rowToHorizontalPosition i
        |> Maybe.andThen
            (\horizontalPosition ->
                (Figure.columnToVerticalPosition j
                    |> Maybe.map (Figure.Position horizontalPosition)
                )
            )
        |> Maybe.map (drawCell model.deck model.draggable)
        |> Maybe.withDefault (Html.text "")


drawCell deck draggable position =
    draggable
        |> DnD.getMeta
        |> Maybe.andThen
            (\(Figure.FigureOnDeck figure position_) ->
                if position == position_ then
                    Just (Html.div [] [])
                else
                    Nothing
            )
        |> Maybe.withDefault
            (case Figure.getFromDeck position deck of
                Just figure ->
                    dnd.draggable (Figure.FigureOnDeck figure position) [] [ View.drawFigure figure ]

                Nothing ->
                    let
                        isMouseOver =
                            case DnD.atDroppable draggable of
                                Just (Dropped position_ _) ->
                                    position_ == position

                                _ ->
                                    False
                    in
                        dnd.droppable (Dropped position)
                            [ classList [ ( ChessCss.OverDrop, isMouseOver ) ] ]
                            []
            )
