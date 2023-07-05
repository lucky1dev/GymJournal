module Planning exposing (..)


import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode
import Json.Decode.Pipeline
import Json.Encode
import Firebase exposing (..)
import Exercises exposing (..)


-- MODEL
type alias Exercise =
    { name : String
    , sets : String
    , reps : String
    , belastung : String
    , start_reps : String
    , start_weight : String
    , reps_now : String 
    , weight_now : String
    }

type alias MessageContent =
    { content : String, time : String, date : String}

type alias WorkoutPlan =
    { userid : String
    , id : Int
    , title : String
    , weekday : String
    , exercises : List Exercise
    }

type ModalMsg
    = InputWorkoutPlan WorkoutPlan

type alias Model =
    { firebase : Firebase.Model 
    , exercises : Exercises.Model
    , trainings : List WorkoutPlan
    , modal : Maybe ModalMsg
    , selectedPlanId : Maybe Int
    , dropdownOpen : Bool
    , messages : List MessageContent}

init : ( Model, Cmd Msg )
init =
    let
        ( exercisesModel, exercisesCmd ) = Exercises.init
    in
    ( {  firebase = Firebase.init
       , exercises = exercisesModel
       , trainings = []
       , modal = Nothing
       , selectedPlanId = Nothing
       , dropdownOpen = False
       , messages = []
       }
    , Cmd.map ExercisesMsg exercisesCmd)


type Msg
    = FirebaseMsg Firebase.Msg
    | ExercisesMsg Exercises.Msg
  --  | DeleteWorkoutPlan Int
    | OpenModal ModalMsg
    | CloseModal
    | UpdateModal ModalMsg
    | SaveModal
    | AddExercise 
    | SelectWorkoutPlan Int
    | ToggleDropdown
    | WorkoutPlansReceived (Result Json.Decode.Error (List WorkoutPlan))


-- UPDATE

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        FirebaseMsg subMsg ->
            let
                (updatedFirebase, cmd) = Firebase.update subMsg model.firebase
            in
            ( { model | firebase = updatedFirebase }, Cmd.map FirebaseMsg cmd )

        ExercisesMsg subMsg ->
            let
                (updatedExercises, cmd) = Exercises.update subMsg model.exercises
            in
            ( { model | exercises = updatedExercises }, Cmd.map ExercisesMsg cmd )
        

 --       DeleteWorkoutPlan id ->
 --           ( { model | saved = List.filter (\plan -> plan.id /= id) model.saved }, Cmd.none )

        OpenModal modalMsg ->
            ( { model | modal = Just modalMsg }, Cmd.none )

        CloseModal ->
            ( { model | modal = Nothing }, Cmd.none )

        UpdateModal modalMsg ->
            ( { model | modal = Just modalMsg }, Cmd.none )

        SaveModal ->
            case model.modal of
                Nothing ->
                    ( model, Cmd.none )
                    
                Just (InputWorkoutPlan plan) ->
                    let
                        uniqueId = 
                            case   List.maximum (List.map .id model.trainings) of
                                Just maxId ->
                                    maxId + 1
                                Nothing ->
                                    1
                                         
                    in
                    (  { model | modal = Nothing }
                        , Firebase.saveWorkoutPlan <| messageEncoderWorkoutplan { plan | id = uniqueId } )

        AddExercise ->
            case model.modal of
                Just (InputWorkoutPlan workoutPlan) ->
                    let
                        updatedWorkoutPlan = { workoutPlan | exercises = workoutPlan.exercises ++ [ { name = "", sets = "", reps = "", belastung = "", start_reps = "", start_weight = "", reps_now = "", weight_now = ""} ] }
                    in
                    ( { model | modal = Just (InputWorkoutPlan updatedWorkoutPlan) }, Cmd.none )

                _ ->
                    ( model, Cmd.none )
    
        ToggleDropdown ->
            ( { model | dropdownOpen = not model.dropdownOpen }, Cmd.none )

        SelectWorkoutPlan id ->
            
            ({ model | selectedPlanId = Just id, dropdownOpen = False }, Cmd.none)

        WorkoutPlansReceived result ->
             case result of
                Ok value ->
                    ( { model | trainings = value }, Cmd.none )

                Err error ->    
                    ( { model | trainings = [] }, Cmd.none )


messageDecoder : Json.Decode.Decoder MessageContent
messageDecoder =
    Json.Decode.succeed MessageContent
        |> Json.Decode.Pipeline.required "content" Json.Decode.string
        |> Json.Decode.Pipeline.required "date" Json.Decode.string
        |> Json.Decode.Pipeline.required "time" Json.Decode.string


messageListDecoder : Json.Decode.Decoder (List MessageContent)
messageListDecoder =
    Json.Decode.list messageDecoder

workoutListDecoder : Json.Decode.Decoder (List WorkoutPlan)
workoutListDecoder =
    Json.Decode.list workoutDecoder

workoutDecoder : Json.Decode.Decoder WorkoutPlan
workoutDecoder =
    Json.Decode.succeed WorkoutPlan
        |> Json.Decode.Pipeline.required "uid" Json.Decode.string
        |> Json.Decode.Pipeline.required "id" Json.Decode.int
        |> Json.Decode.Pipeline.required "title" Json.Decode.string
        |> Json.Decode.Pipeline.required "weekday" Json.Decode.string
        |> Json.Decode.Pipeline.required "exercises" exerciseListDecoder

exerciseListDecoder : Json.Decode.Decoder (List Exercise) 
exerciseListDecoder =
    Json.Decode.list exerciseDecoder

exerciseDecoder : Json.Decode.Decoder Exercise
exerciseDecoder =
    Json.Decode.succeed Exercise
        |> Json.Decode.Pipeline.required "name" Json.Decode.string
        |> Json.Decode.Pipeline.required "sets" Json.Decode.string
        |> Json.Decode.Pipeline.required "reps" Json.Decode.string
        |> Json.Decode.Pipeline.required "belastung" Json.Decode.string
        |> Json.Decode.Pipeline.required "start_reps" Json.Decode.string
        |> Json.Decode.Pipeline.required "start_weight" Json.Decode.string
        |> Json.Decode.Pipeline.required "reps_now" Json.Decode.string
        |> Json.Decode.Pipeline.required "weight_now" Json.Decode.string



messageEncoderWorkoutplan: WorkoutPlan -> Json.Encode.Value
messageEncoderWorkoutplan workoutplan =
    Json.Encode.object
    [ ("uid", Json.Encode.string workoutplan.userid)
    , ( "id", Json.Encode.int workoutplan.id )
    , ( "title", Json.Encode.string workoutplan.title )
    , ( "weekday", Json.Encode.string workoutplan.weekday )
    , ( "exercises", encodeExercises workoutplan.exercises )
        ]

encodeExercises: List Exercise -> Json.Encode.Value
encodeExercises exercises =
    Json.Encode.list encodeExercise exercises

encodeExercise: Exercise -> Json.Encode.Value
encodeExercise exercise =
    Json.Encode.object
        [ ( "name", Json.Encode.string exercise.name )
        , ( "sets", Json.Encode.string exercise.sets )
        , ( "reps", Json.Encode.string exercise.reps )
        , ( "belastung", Json.Encode.string exercise.belastung )
        , ( "start_reps", Json.Encode.string exercise.start_reps )
        , ( "start_weight", Json.Encode.string exercise.start_weight )
        , ( "reps_now", Json.Encode.string exercise.reps_now )
        , ( "weight_now", Json.Encode.string exercise.weight_now )
        ]

planningView : Model -> Html Msg
planningView model =
    div [Html.Attributes.classList [ ( "animate__animated animate__fadeIn", True ) ], style "display" "flex", style "align-items" "center", style "justify-content" "center", style "gap" "20px"] 
        [div [ class "rows" ][ buttonBar model
        , mainView model 
        , case model.modal of
            Nothing ->
                text ""

            Just modalMsg ->
                modalView modalMsg
        ]]

buttonBar : Model -> Html Msg
buttonBar model = 
    let
                            uid = 
                                case model.firebase.userData of
                                    Just data ->
                                        data.uid
                                    Nothing ->
                                        "no uid"

    in 
    div [ class "columns" ]
        [ button [ class "button is-success", style "margin" "20px", onClick (OpenModal (InputWorkoutPlan {userid = uid, id = 1 ,title = "", weekday = "", exercises = [] })) ]
            [ span [] [ text "Add Workoutplan" ]
            
            ]
        , case model.trainings of
             [] ->
                text ""
             _ ->
                createDropDownMenu model
        ]

createDropDownMenu : Model -> Html Msg
createDropDownMenu model =
    let
        isActive = if model.dropdownOpen then "is-active" else ""
        caption =
            if model.dropdownOpen then
                "Trainingsplan auswählen"
            else
                case model.selectedPlanId of
                    Just id ->
                        case List.filter (\plan -> plan.id == id) model.trainings of
                            [selectedPlan] ->
                                (selectedPlan.title ++ " : " ++ selectedPlan.weekday)

                            _ ->
                                "Trainingsplan auswählen"

                    Nothing ->
                        "Trainingsplan auswählen"
        icon = if model.dropdownOpen then "fa fa-angle-up" else "fa fa-angle-down"
    in
    div [ class "level-item" ]
        [ div [ class ("dropdown " ++ isActive) ]
            [ div [ class "dropdown-trigger" ]
                [ button [ class "button is-white"
                         , style "width" "200px"
                         , Html.Attributes.attribute "aria-haspopup" "true"
                         , Html.Attributes.attribute "aria-controls" "dropdown-menu"
                         , onClick ToggleDropdown
                         ]
                    [ span [] [ text caption ]
                    , span [ class "icon is-small" ]
                        [ i [ class icon, Html.Attributes.attribute "aria-hidden" "true" ] [] ]
                    ]
                ]
            , div [ class "dropdown-menu", id "dropdown-menu3", Html.Attributes.attribute "role" "menu" ]
                [ div [ class "dropdown-content" ] (List.concatMap (\plan -> [button [class "button is-white dropdown-item", onClick (SelectWorkoutPlan plan.id)] [text (plan.title ++ " : " ++ plan.weekday)]]) model.trainings) ]
            ]
        ]

mainView : Model -> Html Msg
mainView model =
    let
        selectedPlan = List.filter (\plan -> Just plan.id == model.selectedPlanId) model.trainings
    in
    case selectedPlan of
        [] ->
            text ""

        plan :: _ ->
            div [] [
            div [ class "table-container" ]
                [ table [ class "table is-hoverable" ]
                    [ thead []
                        [ tr []
                            [ th [] [ text "Exercise" ]
                            , th [] [ text "Sets" ]
                            , th [] [ text "Reps" ] 
                            , th [] [ text "Gewicht" ]  -- if exercise = equipped
                            ]
                        ]
                    , tbody []
                        (List.map (\exercise -> 
                            tr []
                            [ td [] [ text exercise.name ]
                            , td [] [ text exercise.sets ]
                            , td [] [ text exercise.reps ]
                            , td [] [ text "Bsp. 50kg" ]
                            , button [class "button is-info"] [text "Gewicht aktualisieren"]  -- if exercise = equipped else Reps aktualisieren
                            ]) plan.exercises)
                    ]
                ]
             , div [] [button [class "button is-danger"][text "Trainingsplan löschen"]]]


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
            [ div [ class "container", style "background-color" "rgba(255, 255, 255, 0.9)", style "display" "flex", style "flex-direction" "column", style "align-items" "center", style "justify-content" "center", style "gap" "10px", style "padding" "20px", style "border-radius" "40px" ]
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