module Planning exposing (..)


import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


-- MODEL

type alias Exercise =
    { name : String
    , sets : String
    , reps : String
    }

type alias WorkoutPlan =
    { id : Int
    , title : String
    , weekday : String
    , exercises : List Exercise
    }

type alias Model =
    { nextId : Int
    , saved : List WorkoutPlan
    , modal : Maybe ModalMsg
    }

type ModalMsg
    = InputWorkoutPlan WorkoutPlan

type Msg
    = SaveWorkoutPlan WorkoutPlan
    | DeleteWorkoutPlan Int
    | OpenModal ModalMsg
    | CloseModal
    | UpdateModal ModalMsg
    | UpdateWorkoutPlan Int WorkoutPlan
    | UpdateExercise Int Int Exercise
    | SaveModal
    | AddExercise 

init : Model
init =
    { nextId = 0
    , saved = []
    , modal = Nothing
    }

-- UPDATE

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        SaveWorkoutPlan workoutPlan ->
            ( { model | saved = workoutPlan :: model.saved }, Cmd.none )

        DeleteWorkoutPlan id ->
            ( { model | saved = List.filter (\plan -> plan.id /= id) model.saved }, Cmd.none )

        OpenModal modalMsg ->
            ( { model | modal = Just modalMsg }, Cmd.none )

        CloseModal ->
            ( { model | modal = Nothing }, Cmd.none )

        UpdateModal modalMsg ->
            ( { model | modal = Just modalMsg }, Cmd.none )

        UpdateWorkoutPlan id updatedPlan ->
            let
                updatedSaved = List.indexedMap (\idx plan -> if idx == id then { plan | title = updatedPlan.title, weekday = updatedPlan.weekday } else plan) model.saved
            in
            ( { model | saved = updatedSaved }, Cmd.none )

        UpdateExercise id exerciseId updatedExercise ->
            let
                updatedSaved = List.indexedMap (\idx plan -> if idx == id then { plan | exercises = List.indexedMap (\idx2 ex -> if idx2 == exerciseId then updatedExercise else ex) plan.exercises } else plan) model.saved
            in
            ( { model | saved = updatedSaved }, Cmd.none )
        
        SaveModal ->
            case model.modal of
                Nothing ->
                    ( model, Cmd.none )
                    
                Just (InputWorkoutPlan plan) ->
                    ( { model | saved = plan :: model.saved, modal = Nothing }, Cmd.none )

        AddExercise ->
            case model.modal of
                Just (InputWorkoutPlan workoutPlan) ->
                    let
                        updatedWorkoutPlan = { workoutPlan | exercises = workoutPlan.exercises ++ [ { name = "", sets = "", reps = "" } ] }
                    in
                    ( { model | modal = Just (InputWorkoutPlan updatedWorkoutPlan) }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

-- VIEW





planningView : Model -> Html Msg
planningView model =
    div [] 
        [ buttonBar
        , mainView model
        ]

buttonBar: Html Msg
buttonBar = 
    div [] [ button [ class "button is-white is-fullwidth", onClick (OpenModal (InputWorkoutPlan { id = 0, title = "", weekday = "", exercises = [] }))  ] [ text "Trainingsplan hinzufügen" ]]



mainView : Model -> Html Msg
mainView model =
    div []
        [ div [] (List.indexedMap planView model.saved)
        , case model.modal of
            Nothing ->
                text ""

            Just modalMsg ->
                modalView modalMsg
        ]

planView : Int -> WorkoutPlan -> Html Msg
planView id plan =
    div []
        [ div [] [ text plan.title ]
        , button [ class "button is-danger", onClick (OpenModal (InputWorkoutPlan plan)) ] [ text "Edit" ]
        , button [ class "button is-danger", onClick (DeleteWorkoutPlan id) ] [ text "Delete" ]
        , div [] (List.indexedMap exerciseView plan.exercises)
        ]

exerciseView : Int -> Exercise -> Html Msg
exerciseView id exercise =
    div []
        [ div [] [ text exercise.name ]
        ]

modalView : ModalMsg -> Html Msg
modalView modalMsg =
    case modalMsg of
        InputWorkoutPlan plan ->
            inputWorkoutPlanModal plan

inputWorkoutPlanModal : WorkoutPlan -> Html Msg
inputWorkoutPlanModal plan =
    div [ class "modal is-active"]
        [ div [ class "modal-background" ] []
        , div [ class "modal-content" ]
            [ div [ class "container", style "background-color" "white", style "display" "flex", style "flex-direction" "column", style "align-items" "center", style "justify-content" "center", style "gap" "10px", style "padding" "20px" ]
                [ p [class "is-size-4", style "color" "#333"] [text "Name"] 
                , input [ class "input is-rounded", type_ "text", value plan.title, onInput (\newTitle -> UpdateModal (InputWorkoutPlan { plan | title = newTitle })), style "border-radius" "25px", style "border" "2px solid #ccc", style "padding" "10px" ] []
                , p [class "is-size-4", style "color" "#333"] [text "Wochentag"] 
                , input [ class "input is-rounded ", type_ "text", value plan.weekday, onInput (\newWeekday -> UpdateModal (InputWorkoutPlan { plan | weekday = newWeekday })), style "border-radius" "25px", style "border" "2px solid #ccc", style "padding" "10px" ] []
                , div [] (List.indexedMap (\idx exercise -> inputExerciseInModal idx plan exercise) plan.exercises)
                , div [ style "display" "flex", style "justify-content" "center", style "align-items" "center", style "gap" "10px" ]
                    [ button [ class "button is-danger", onClick AddExercise, style "border-radius" "25px" ] [ text "Übung hinzufügen" ] 
                    , button [ class "button is-success", onClick SaveModal, style "border-radius" "25px" ] [ text "Speichern" ]
                    , button [ class "button", onClick CloseModal, style "border-radius" "25px" ] [ text "Schließen" ]
                    ]
                ]
            ]  
        ]



inputExerciseInModal : Int -> WorkoutPlan -> Exercise -> Html Msg
inputExerciseInModal idx plan exercise =
    div [ style "display" "flex", style "flex-direction" "row", style "gap" "10px"]
        [ div [ class "field" ]
            [ label [ class "label" ] [ text "Übung" ]
            , div [ class "control" ]
                [ input [ class "input", type_ "text", value exercise.name, onInput (\newName -> UpdateModal (InputWorkoutPlan { plan | exercises = List.indexedMap (\i ex -> if i == idx then { ex | name = newName } else ex) plan.exercises })) ] []
                ]
            ]
        , div [ class "field" ]
            [ label [ class "label" ] [ text "Sets" ]
            , div [ class "control" ]
                [ input [ class "input", type_ "text", value exercise.sets, onInput (\newSets -> UpdateModal (InputWorkoutPlan { plan | exercises = List.indexedMap (\i ex -> if i == idx then { ex | sets = newSets } else ex) plan.exercises })) ] []
                ]
            ]
        , div [ class "field" ]
            [ label [ class "label" ] [ text "Wiederholungen" ]
            , div [ class "control" ]
                [ input [ class "input", type_ "text", value exercise.reps, onInput (\newReps -> UpdateModal (InputWorkoutPlan { plan | exercises = List.indexedMap (\i ex -> if i == idx then { ex | reps = newReps } else ex) plan.exercises })) ] []
                ]
            ]
        ]