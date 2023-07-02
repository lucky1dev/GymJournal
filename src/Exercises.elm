module Exercises exposing (..)

import Http
import Json.Decode as Decode
import Html exposing (..)
import Html.Attributes exposing (..)


-- Typendefinitionen

type alias Training =
    { name : String
    , hauptmuskelgruppe : String
    , equipment : String
    , erklaerung : String
    , youtubeLink : String
    }

type alias Model =
    { trainings : List Training
    , state : HttpState
    }

type HttpState
    = Failure String
    | Loading
    | Success


-- JSON Decoder

trainingDecoder : Decode.Decoder Training
trainingDecoder =
    Decode.map5 Training
        (Decode.field "Übungsname" Decode.string)
        (Decode.field "Hauptmuskelgruppe" Decode.string)
        (Decode.field "Equipment" Decode.string)
        (Decode.field "Erklärung" Decode.string)
        (Decode.field "YoutubeLink" Decode.string)


-- Anfangszustand

init : ( Model, Cmd Msg )
init =
    ( Model [] Loading, getTrainings )

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

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotTrainings result ->
            case result of
                Ok trainings ->
                    ( { model | trainings = trainings, state = Success }, Cmd.none )

                Err _ ->
                    ( { model | state = Failure "Fehler beim Laden der Trainings" }, Cmd.none )


-- View Funktion

exercisesView : Model -> Html Msg
exercisesView model =
    div []
        [ h2 [] [ text "Trainingsübungen" ]
        , ul [] (List.map viewTraining model.trainings)
        ]

viewTraining : Training -> Html Msg
viewTraining training =
    li [] [ text training.name ]