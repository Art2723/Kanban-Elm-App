module Main exposing (main)

import Browser
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes as Attr exposing (..)
import Html.Events as Events exposing (onClick, onInput)
import Json.Decode as Decode


colors : Dict String String
colors =
    Dict.fromList
        [ ( "White", "white" )
        , ( "Cyan", "lightcyan" )
        , ( "Green", "lightgreen" )
        , ( "Salmon", "lightsalmon" )
        , ( "Lemon", "lemonchiffon" )
        , ( "Blue", "lightblue" )
        , ( "Pink", "lightpink" )
        ]


columnNames : List String
columnNames =
    [ "To Do"
    , "Do Today"
    , "In Progress"
    , "Done"
    ]


type alias Card =
    { cardname : String
    , id : Int
    , cardnote : String
    , cardcolor : String
    , columnname : String
    }


emptyCard : Card
emptyCard =
    { cardname = "", id = 0, cardnote = "", cardcolor = "White", columnname = "To Do" }


type alias Model =
    { cards : Dict Int Card
    , modalParams :
        Card
    , visible : Bool
    , beingDragged : Maybe Int
    , columnToDrop : String
    }


type Msg
    = RemoveCard Int
    | NewCard String
    | CloseModal
    | EditCard Int
    | SaveCard Card
    | ChangeModalParam String String
    | DoNothing
    | Drag Int
    | DragEnd
    | DragOver String
    | Drop


initialModel : Model
initialModel =
    { cards =
        Dict.fromList
            [ ( 1
              , { cardname = "First card"
                , id = 1
                , cardnote = "Do the homework"
                , cardcolor = "Green"
                , columnname = "To Do"
                }
              )
            , ( 2
              , { cardname = "Second card"
                , id = 2
                , cardnote = "Make first React app"
                , cardcolor = "Cyan"
                , columnname = "In Progress"
                }
              )
            , ( 3
              , { cardname = "third card"
                , id = 3
                , cardnote = "Walk with my dog"
                , cardcolor = "Salmon"
                , columnname = "Done"
                }
              )
            , ( 4
              , { cardname = "Forth card"
                , id = 4
                , cardnote = "Make a dinner"
                , cardcolor = "Lemon"
                , columnname = "In Progress"
                }
              )
            , ( 5
              , { cardname = "Fifth card"
                , id = 5
                , cardnote = "Improve skills in Vim, Tmux"
                , cardcolor = "Blue"
                , columnname = "Done"
                }
              )
            , ( 6
              , { cardname = "Sixth card"
                , id = 6
                , cardnote = "Save the World"
                , cardcolor = "Pink"
                , columnname = "To Do"
                }
              )
            ]
    , modalParams = emptyCard
    , visible = False
    , beingDragged = Nothing
    , columnToDrop = "To Do"
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel
    , Cmd.none
    )


view : Model -> Html Msg
view model =
    div [ class "app" ]
        [ h1 [] [ text "Kanban board" ]
        , div [ class "kanban-board " ]
            (List.map (viewColumn model) columnNames)
        , viewModal model.modalParams model.visible
        ]


viewColumn : Model -> String -> Html Msg
viewColumn model columnName =
    div [ class "kanban-column" ]
        [ div [ class "kanban-column-name" ]
            [ text columnName
            , button [ class "new-card-btn", onClick (NewCard columnName) ] [ text "+" ]
            ]
        , div
            [ class "kanban-column-area"
            , onDragOver (DragOver columnName)
            , onDrop Drop
            ]
            (Dict.values model.cards
                |> List.filter (\card -> card.columnname == columnName)
                |> List.map viewCard
            )
        ]


viewCard : Card -> Html Msg
viewCard card =
    div
        [ class "kanban-card"
        , style "backgroundColor" (colorNameToColor card.cardcolor)
        , Attr.draggable "true"
        , onDragStart <| Drag card.id
        , onDragEnd DragEnd
        ]
        [ div [ class "kanban-card-name", onClick (EditCard card.id) ] [ text card.cardname ]
        , button [ class "remove-card-btn", onClick (RemoveCard card.id) ] [ text "x" ]
        , div [ class "kanban-card-note", onClick (EditCard card.id) ] [ text card.cardnote ]
        , br [] []
        , span [ onClick (SaveCard card), onClick (EditCard card.id) ] [ text ("Column: " ++ card.columnname) ]
        ]


getId : Model -> Int
getId model =
    model.cards
        |> Dict.size
        |> (+) 1


viewModal : Card -> Bool -> Html Msg
viewModal ({ cardname, cardnote, cardcolor, columnname } as card) visible =
    div [ classList [ ( "active", visible ) ], class "kanban-modal", onClick CloseModal ]
        [ div [ class "kanban-modal-content", onClickStopPropagation DoNothing ]
            [ div []
                [ input [ class "kanban-input", Attr.placeholder "Title", value cardname, onInput (ChangeModalParam "title") ] []
                , input [ class "kanban-input", Attr.placeholder "Note", value cardnote, onInput (ChangeModalParam "note") ] []
                , div [ class "color-select" ]
                    [ label [ for "color-select" ] [ text "Color:" ]
                    , select [ class "kanban-select", name "color-select", onInput (ChangeModalParam "color") ]
                        (List.map (\colorInList -> option [ value colorInList, selected (colorInList == cardcolor) ] [ text colorInList ]) (Dict.keys colors))
                    ]
                , div [ class "col-select" ]
                    [ label [ for "col-select" ] [ text "Column:" ]
                    , select [ class "kanban-select", name "col-select", onInput (ChangeModalParam "column") ]
                        (List.map (\columnInList -> option [ value columnInList, selected (columnInList == columnname) ] [ text columnInList ]) columnNames)
                    ]
                , div [ class "buttons" ]
                    [ button [ class "kanban-button", onClick (SaveCard card) ] [ text "Save" ]
                    , button [ class "kanban-button", onClick CloseModal ] [ text "Cancel" ]
                    ]
                ]
            ]
        ]


colorNameToColor : String -> String
colorNameToColor colorName =
    case Dict.get colorName colors of
        Just color ->
            color

        Nothing ->
            "white"


onClickStopPropagation : a -> Attribute a
onClickStopPropagation msg =
    Events.stopPropagationOn "click" <| Decode.succeed ( msg, True )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RemoveCard id ->
            ( { model | cards = Dict.remove id model.cards }, Cmd.none )

        NewCard columnName ->
            ( { model | modalParams = { emptyCard | columnname = columnName, id = getId model }, visible = True }, Cmd.none )

        CloseModal ->
            ( { model | visible = False, modalParams = emptyCard }, Cmd.none )

        EditCard id ->
            let
                card =
                    getCard id model
            in
            ( { model | visible = True, modalParams = card}, Cmd.none )

        SaveCard card ->
            ( { model | modalParams = emptyCard, cards = Dict.insert card.id card model.cards, visible = False }, Cmd.none )

        ChangeModalParam param val ->
            let
                modalParams =
                    model.modalParams
            in
            case param of
                "title" ->
                    ( { model | modalParams = { modalParams | cardname = val } }, Cmd.none )

                "note" ->
                    ( { model | modalParams = { modalParams | cardnote = val } }, Cmd.none )

                "color" ->
                    ( { model | modalParams = { modalParams | cardcolor = val } }, Cmd.none )

                "column" ->
                    ( { model | modalParams = { modalParams | columnname = val } }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        DoNothing ->
            ( model, Cmd.none )

        Drag cardId ->
            ( { model | beingDragged = Just cardId }, Cmd.none )

        DragEnd ->
            ( { model | beingDragged = Nothing }, Cmd.none )

        DragOver columnName ->
            ( { model | columnToDrop = columnName }, Cmd.none )

        Drop ->
            case model.beingDragged of
                Nothing ->
                    ( model, Cmd.none )

                Just cardId ->
                    case Dict.get cardId model.cards of
                        Just card ->
                            -- ( { model | beingDragged = Nothing, cards = Dict.update cardId (Maybe.map <| \_ -> { card | columnname = model.columnToDrop }) model.cards }, Cmd.none )
                            ( { model | beingDragged = Nothing, cards = Dict.insert cardId card model.cards }, Cmd.none )

                        Nothing ->
                            ( model, Cmd.none )


getCard : Int -> Model -> Card
getCard id model =
    case Dict.get id model.cards of
        Just card ->
            card

        Nothing ->
            emptyCard


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


onDragStart : a -> Attribute a
onDragStart msg =
    Events.on "dragstart" <|
        Decode.succeed msg


onDragEnd : a -> Attribute a
onDragEnd msg =
    Events.on "dragend" <|
        Decode.succeed msg


onDragOver : a -> Attribute a
onDragOver msg =
    Events.preventDefaultOn "dragover" <|
        Decode.succeed ( msg, True )


onDrop : a -> Attribute a
onDrop msg =
    Events.preventDefaultOn "drop" <|
        Decode.succeed ( msg, True )
