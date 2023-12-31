module Exercises exposing (..)

import Http
import Json.Decode as Decode
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List

type alias Training =
    { name : String
    , hauptmuskelgruppe : String
    , belastung : String
    , erklaerung : String
    }

type HttpState
    = Failure String
    | Loading
    | Success

type alias Model =
    { trainings : List Training
    , state : HttpState
    , belastungFilter : String
    , hauptmuskelgruppeFilter : String
    , selectedTraining : Maybe Training
    }


init : ( Model, Cmd Msg )
init =
    ( Model [] Loading "" "" Nothing, getTrainings )


trainingDecoder : Decode.Decoder Training
trainingDecoder =
    Decode.map4 Training
        (Decode.field "Übungsname" Decode.string)
        (Decode.field "Hauptmuskelgruppe" Decode.string)
        (Decode.field "Belastung" Decode.string)
        (Decode.field "Erklärung" Decode.string)


getTrainings : Cmd Msg
getTrainings =
    Http.get
        { url = "https://raw.githubusercontent.com/lucky1dev/GymJournal/main/public/exercises.JSON"
        , expect = Http.expectJson GotTrainings (Decode.field "Trainingsübungen" (Decode.list trainingDecoder))
        }


type Msg
    = GotTrainings (Result Http.Error (List Training))
    | UpdateBelastungFilter String
    | UpdateHauptmuskelgruppeFilter String
    | OpenTrainingModal (Maybe Training)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotTrainings result ->
            case result of
                Ok trainings ->
                    let sortedTrainings = List.sortBy .name trainings
                    in ( { model | trainings = sortedTrainings, state = Success }, Cmd.none )

                Err _ ->
                    ( { model | state = Failure "Fehler beim Laden der Trainings" }, Cmd.none )


        UpdateBelastungFilter newFilter ->
            ( { model | belastungFilter = newFilter }, Cmd.none )

        UpdateHauptmuskelgruppeFilter newFilter ->
            ( { model | hauptmuskelgruppeFilter = newFilter }, Cmd.none )

        OpenTrainingModal training ->
            ( { model | selectedTraining = training }, Cmd.none )


exercisesView : Model -> Html Msg
exercisesView model =
    div []
        [  div [         style "display" "flex", style "flex-direction" "row", style "justify-content" "center", style "align-items" "center",  style "position" "relative"
                    , style "z-index" "2"] 
            [ div [ class "panel-heading", style "display" "flex", style "background" "transparent", Html.Attributes.classList [ ( "animate__animated animate__fadeInDown", True ) ]
            ] [ div [class "select is-info is-rounded is-small", style "margin-right" "10px"]
                [ select [ onInput UpdateBelastungFilter

                        ]
                    [ option [ value ""] [ text "Alle Übungsarten" ]
                    , option [ value "Freihantel" ] [ text "Freihantel" ]
                    , option [ value "Körpergewicht" ] [ text "Körpergewicht" ]
                    , option [ value "Maschine" ] [ text "Maschine" ]
                    ]]
                 ,div [class"select is-info is-rounded is-small", style "margin-left" "10px"
                 ]
                    [ select [ onInput UpdateHauptmuskelgruppeFilter
                        ]
                    [ option [ value ""] [ text "Alle Muskelgruppen" ]
                    , option [ value "Beine" ] [ text "Beine" ]
                    , option [ value "Brust" ] [ text "Brust" ]
                    , option [ value "Rücken" ] [ text "Rücken" ]
                    , option [ value "Schultern" ] [ text "Schultern" ]
                    , option [ value "Bauch" ] [ text "Bauch" ]
                    ]
                ]]]
        , div [ class "panel scrollable-panel"
        , style "display" "flex"
        , style "flex-direction" "column"
        , style "position" "center"
        , style "align-items" "center"
        ,Html.Attributes.classList [ ( "animate__animated animate__fadeInUp", True ) ] ]
            (List.filter (\training -> (model.belastungFilter == "" || training.belastung == model.belastungFilter) && (model.hauptmuskelgruppeFilter == "" || training.hauptmuskelgruppe == model.hauptmuskelgruppeFilter)) model.trainings |> List.map viewTraining)
        , trainingModalView model.selectedTraining
        ]

viewTraining : Training -> Html Msg
viewTraining training =
    div
        [ class "panel-block ",style "width" "400px", onClick (OpenTrainingModal (Just training))
        ]
        [ div [ class "content", style "width" "200px", style "margin-left" "100px"]
            [ b [ class "title is-4 has-text-white" ] [ text training.name ]
            , p [style "margin" "0px"][b [ class "subtitle is-6" ] [ text ("Muskkelgruppe:  " ++ training.hauptmuskelgruppe) ]]
            , b [class "subtitle is-6"] [ text ("Übungsart:  " ++  training.belastung) ]
            ]
        ]

trainingModalView : Maybe Training -> Html Msg
trainingModalView maybeTraining =
    case maybeTraining of
        Nothing ->
            text ""

        Just training ->
            div [ class "modal is-active"]
                [ div [ class "modal-background" ,onClick (OpenTrainingModal Nothing)]
                    []
                , div 
                            [ class "modal-card"   
                            , style "border" "none"
                            , style "margin" "15px auto"
                            , style "max-width" "380px"
                            , style "border-radius" "6px"
                            , style "box-shadow" "0 3px 7px rgba(0, 0, 0, 0.3)"
                            , Html.Attributes.classList [ ( "animate__animated animate__zoomIn", True )  ] 
                            ]
                            [ div [ class "modal-card-head custom-modal-header is-flex is-justify-content-space-between" ]
                                [ p [] [ span [ class "title is-4 has-text-black" ] [ text training.name ] ]
                            --    , a [ class "button is-danger" ,href "https://www.youtube.com/watch?v=50bRdFkkm4I" ] [ text "Youtube Link"] 
                                 ] 

                            , div [ class "modal-card-body custom-modal-body" ]
                                [ p [ style "margin-bottom" "20px" ]
                                    [ span [ class "title is-5 has-text-black" ] [ text "Hauptmuskelgruppe:     " ]
                                    , span [ class "title is-5 has-text-black" ] [ text training.hauptmuskelgruppe ]
                                    ]
                                , p [ style "margin-bottom" "30px" ]
                                    [ span [ class "title is-5 has-text-black" ] [ text "Übungsart:     " ]
                                    , span [ class "title is-5 has-text-black" ] [ text training.belastung ]
                                    ]
                                , p [ style "margin-bottom" "20px" ]
                                    [ p [ class "title is-5 has-text-black" ] [ text "Ausführung" ]
                                    , p [ class "subtitle is-5 has-black" ] [ text training.erklaerung ]
                                    ]
                                ]
                                ]]



