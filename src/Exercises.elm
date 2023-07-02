module Exercises exposing (..)

import Http
import Json.Decode as Decode
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


-- Typendefinitionen

type alias Training =
    { name : String
    , hauptmuskelgruppe : String
    , equipment : String
    , erklaerung : String
    , youtubeLink : String
    }

type HttpState
    = Failure String
    | Loading
    | Success

type alias Model =
    { trainings : List Training
    , state : HttpState
    , equipmentFilter : String
    , hauptmuskelgruppeFilter : String
    , selectedTraining : Maybe Training
    }

-- Anfangszustand

init : ( Model, Cmd Msg )
init =
    ( Model [] Loading "" "" Nothing, getTrainings )


-- JSON Decoder

trainingDecoder : Decode.Decoder Training
trainingDecoder =
    Decode.map5 Training
        (Decode.field "Übungsname" Decode.string)
        (Decode.field "Hauptmuskelgruppe" Decode.string)
        (Decode.field "Equipment" Decode.string)
        (Decode.field "Erklärung" Decode.string)
        (Decode.field "YoutubeLink" Decode.string)


-- HTTP Anfrage

getTrainings : Cmd Msg
getTrainings =
    Http.get
        { url = "./exercises.json"
        , expect = Http.expectJson GotTrainings (Decode.field "Trainingsübungen" (Decode.list trainingDecoder))
        }


-- Update Funktion

type Msg
    = GotTrainings (Result Http.Error (List Training))
    | UpdateEquipmentFilter String
    | UpdateHauptmuskelgruppeFilter String
    | OpenTrainingModal (Maybe Training)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotTrainings result ->
            case result of
                Ok trainings ->
                    ( { model | trainings = trainings, state = Success }, Cmd.none )

                Err _ ->
                    ( { model | state = Failure "Fehler beim Laden der Trainings" }, Cmd.none )

        UpdateEquipmentFilter newFilter ->
            ( { model | equipmentFilter = newFilter }, Cmd.none )

        UpdateHauptmuskelgruppeFilter newFilter ->
            ( { model | hauptmuskelgruppeFilter = newFilter }, Cmd.none )

        OpenTrainingModal training ->
            ( { model | selectedTraining = training }, Cmd.none )

-- View Funktion

exercisesView : Model -> Html Msg
exercisesView model =
    div []
        [ h2 [] [ text "Trainingsübungen" ]
        , div [ class "panel" ]
            [ p [ class "panel-heading" ] [ text "Filter" ]
            , div [ class "panel-block" ]
                [ select [ onInput UpdateEquipmentFilter ]
                    [ option [ value ""] [ text "Alle" ]
                    , option [ value "Freihantel" ] [ text "Freihantel" ]
                    , option [ value "Körpergewicht" ] [ text "Körpergewicht" ]
                    ]
                ]
            , div [ class "panel-block" ]
                [ select [ onInput UpdateHauptmuskelgruppeFilter ]
                    [ option [ value ""] [ text "Alle" ]
                    , option [ value "Beine" ] [ text "Beine" ]
                    , option [ value "Brust" ] [ text "Brust" ]
                    , option [ value "Rücken" ] [ text "Rücken" ]
                    , option [ value "Schultern" ] [ text "Schultern" ]
                    , option [ value "Bauch" ] [ text "Bauch" ]
                    ]
                ]
            , p [ class "panel-tabs" ] []
            ]
        , div [ class "panel scrollable-panel" ]
            (List.filter (\training -> (model.equipmentFilter == "" || training.equipment == model.equipmentFilter) && (model.hauptmuskelgruppeFilter == "" || training.hauptmuskelgruppe == model.hauptmuskelgruppeFilter)) model.trainings |> List.map viewTraining)
        , trainingModalView model.selectedTraining
        ]


viewTraining : Training -> Html Msg
viewTraining training =
    div
        [ class "panel-block"
        , onClick (OpenTrainingModal (Just training))
        ]
        [ div [ class "content" ]
            [ p [ class "title is-4" ] [ text training.name ]
            , p [ class "subtitle is-6" ] [ text training.hauptmuskelgruppe ]
            , p [] [ text training.equipment ]
            ]
        ]


trainingModalView : Maybe Training -> Html Msg
trainingModalView maybeTraining =
    case maybeTraining of
        Nothing ->
            text ""

        Just training ->
            div [ class "modal is-active" ]
                [ div [ class "modal-background" ]
                    []
                , div [ class "modal-content" ]
                    [ p [] [ text training.name ]
                    , p [] [ text training.hauptmuskelgruppe ]
                    , p [] [ text training.equipment ]
                    , p [] [ text training.erklaerung ]
                    , a [ href training.youtubeLink ] [ text "Youtube Link" ]
                    ]
                , button [ class "modal-close is-large", style "ariaLabel" "close", onClick (OpenTrainingModal Nothing) ] []
                ]


