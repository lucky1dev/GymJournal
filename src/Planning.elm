module Planning exposing (..)


import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode
import Json.Decode.Pipeline
import Json.Encode
import Firebase exposing (..)
import Exercises exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)

-- MODEL
type alias Exercise =
    { name : String
    , sets : String
    , reps : String
    , belastung : String
    , start_weight : Float
    , weight_now : Float
    }


type alias WorkoutPlan =
    { userid : String
    , id : Int
    , title : String
    , weekday : String
    , exercises : List Exercise
    }

type ModalState 
    = DelPlan Int
    | ChangeLoad WorkoutPlan Exercise


type alias Model =
    { firebase : Firebase.Model 
    , exercises : Exercises.Model
    , trainings : List WorkoutPlan
    , selectedPlanId : Maybe Int
    , selectedExerciseIndex : Maybe Int
    , dropdownOpen : Bool
    , newPlan : Maybe WorkoutPlan
    , modal : Maybe ModalState
    , newWeight : Maybe Float
    }

type ModalMsg
    = OpenDeletePlan Int 
    | OpenChangeLoadExercise WorkoutPlan Exercise Int

init : ( Model, Cmd Msg )
init =
    let
        ( exercisesModel, exercisesCmd ) = Exercises.init
    in
    ( {  firebase = Firebase.init
       , exercises = exercisesModel
       , trainings = []
       , selectedPlanId = Nothing
       , selectedExerciseIndex = Nothing
       , dropdownOpen = False
       , newPlan = Nothing
       , modal = Nothing
       , newWeight = Nothing
       }
    , Cmd.map ExercisesMsg exercisesCmd)



type Msg
    = FirebaseMsg Firebase.Msg
    | ExercisesMsg Exercises.Msg
    | DeleteWorkoutPlan Int
    | OpenInput 
    | CloseInput
    | UpdateInput WorkoutPlan
    | SaveInput WorkoutPlan
    | AddExercise WorkoutPlan 
    | RemoveExercise WorkoutPlan Int
    | SelectWorkoutPlan Int
    | ToggleDropdown
    | WorkoutPlansReceived (Result Json.Decode.Error (List WorkoutPlan))
    | OpenModal ModalMsg 
    | CloseModal
    | UpdateExerciseWeight 
    | UpdateNewWeightString String


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
        

        DeleteWorkoutPlan id ->
            let idString = String.fromInt id
            in
            ( {model |modal = Nothing} , Firebase.deleteWorkoutPlan idString )

        OpenInput  ->
            let
                            uid = 
                                case model.firebase.userData of
                                    Just data ->
                                        data.uid
                                    Nothing ->
                                        "no uid"

             in 
            ( { model | newPlan = Just {userid = uid, id = 1 ,title = "", weekday = "", exercises = [] } , selectedPlanId = Nothing }, Cmd.none )

        CloseInput ->
            ( { model | newPlan = Nothing }, Cmd.none )

        UpdateInput workoutplan ->
            ( { model | newPlan = Just workoutplan }, Cmd.none )

        SaveInput workoutplan ->
                    let
                        uniqueId = 
                            case   List.maximum (List.map .id model.trainings) of
                                Just maxId ->
                                    maxId + 1
                                Nothing ->
                                    1
                                         
                    in
                    (  { model | newPlan = Nothing }
                        , Firebase.saveWorkoutPlan <| messageEncoderWorkoutplan { workoutplan | id = uniqueId } )

        AddExercise workoutplan ->
            let
                emptyExercise = { name = "", sets = "", reps = "", belastung = "", start_weight = 0 , weight_now = 0}
                updatedExercises = workoutplan.exercises ++ [emptyExercise]
                updatedWorkoutPlan = { workoutplan | exercises = updatedExercises }
            in
            ( { model | newPlan =  Just updatedWorkoutPlan }, Cmd.none )


        RemoveExercise workoutPlan index ->
            let
                updatedExercises = List.take index workoutPlan.exercises ++ List.drop (index + 1) workoutPlan.exercises
                updatedWorkoutPlan = { workoutPlan | exercises = updatedExercises }
            in
            ( { model | newPlan = Just updatedWorkoutPlan }, Cmd.none )
    
        ToggleDropdown ->
            ( { model | dropdownOpen = not model.dropdownOpen }, Cmd.none )

        SelectWorkoutPlan id ->
            
            ({ model | selectedPlanId = Just id, dropdownOpen = False, newPlan = Nothing }, Cmd.none)

        WorkoutPlansReceived result ->
             case result of
                Ok value ->
                    ( { model | trainings = value }, Cmd.none )

                Err error ->    
                    ( { model | trainings = [] }, Cmd.none )

        OpenModal modalMsg ->
            case modalMsg of
                OpenDeletePlan id ->
                    ( { model | modal = Just (DelPlan id) }, Cmd.none )

                OpenChangeLoadExercise plan exercise int -> 
                    ( { model | modal = Just (ChangeLoad plan exercise), selectedExerciseIndex = Just int }, Cmd.none )

        
        CloseModal ->
            ( { model | modal = Nothing }, Cmd.none )

        UpdateExerciseWeight ->
            case (model.selectedPlanId, model.selectedExerciseIndex, model.newWeight) of
                (Just planId, Just exerciseIndex, Just newWeight) ->
                    let
                        maybePlanToUpdate =
                            List.filter (\plan -> plan.id == planId) model.trainings
                                |> List.head

                        updatedPlan =
                            case maybePlanToUpdate of
                                Just plan ->
                                    let
                                        updatedExercises =
                                            List.indexedMap
                                                (\idx exercise ->
                                                    if idx == exerciseIndex then
                                                        let
                                                            updatedStartWeight = 
                                                                if exercise.start_weight == 0 then
                                                                    newWeight
                                                                else
                                                                    exercise.start_weight
                                                        in
                                                        { exercise | start_weight = updatedStartWeight, weight_now = newWeight }
                                                    else
                                                        exercise
                                                )
                                                plan.exercises
                                    in
                                    Just { plan | exercises = updatedExercises }

                                Nothing ->
                                    Nothing

                        updatedFirebase =
                            case updatedPlan of
                                Just plan ->
                                    Firebase.saveWorkoutPlan <| messageEncoderWorkoutplan plan

                                Nothing ->
                                    Cmd.none
                    in
                    ( { model | trainings = List.map (\plan -> if plan.id == planId then Maybe.withDefault plan updatedPlan else plan) model.trainings, modal = Nothing, newWeight = Nothing }
                    , Cmd.map FirebaseMsg updatedFirebase
                    )

                _ ->
                    ( model, Cmd.none )



        UpdateNewWeightString newWeightString ->
            let
                newWeight = String.toFloat newWeightString
            in
            ({ model | newWeight = newWeight }, Cmd.none)




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
        |> Json.Decode.Pipeline.required "start_weight" Json.Decode.float
        |> Json.Decode.Pipeline.required "weight_now" Json.Decode.float

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
        , ( "start_weight", Json.Encode.float exercise.start_weight )
        , ( "weight_now", Json.Encode.float exercise.weight_now )
        ]

planningView : Model -> Html Msg
planningView model =
    div [Html.Attributes.classList [ ( "animate__animated animate__fadeIn", True ) ], Html.Attributes.style "display" "flex", Html.Attributes.style "align-items" "center", Html.Attributes.style "justify-content" "center", Html.Attributes.style "gap" "20px"] 
        [div [ Html.Attributes.class "rows" ][ buttonBar model
        , planView model
        , inputWorkoutPlanView model
        , modalView model
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
    div [ Html.Attributes.class "level-item" ]
        [ button [ Html.Attributes.class "button is-info is-responsive", Html.Attributes.style "margin" "10px", onClick OpenInput ]
            [ Html.text "Trainingsplan erstellen" ]  
            
        , case model.trainings of
             [] ->
                Html.text ""
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
    div [ Html.Attributes.class "level-item" ]
        [ div [ Html.Attributes.class ("dropdown " ++ isActive), Html.Attributes.style "margin" "10px"]
            [ div [ Html.Attributes.class "dropdown-trigger" ]
                [ button [ Html.Attributes.class "button is-info is-responsive "
                         , Html.Attributes.style "width" "210px"
                         , Html.Attributes.attribute "aria-haspopup" "true"
                         , Html.Attributes.attribute "aria-controls" "dropdown-menu"
                         , onClick ToggleDropdown
                         ]
                    [ span [] [ Html.text caption ]
                    , span [ Html.Attributes.class "icon is-small" ]
                        [ i [ Html.Attributes.class icon, Html.Attributes.attribute "aria-hidden" "true" ] [] ]
                    ]
                ]
            , div [ Html.Attributes.class "dropdown-menu", Html.Attributes.id "dropdown-menu3", Html.Attributes.attribute "role" "menu" ]
                [ div [ Html.Attributes.class "dropdown-content" ] (List.concatMap (\plan -> [button [Html.Attributes.class "button is-white dropdown-item is-responsive", onClick (SelectWorkoutPlan plan.id)] [Html.text (plan.title ++ " : " ++ plan.weekday)]]) model.trainings) ]
            ]
        ]
planView : Model -> Html Msg
planView model =
    let
        selectedPlan = List.filter (\plan -> Just plan.id == model.selectedPlanId) model.trainings

    in
    case selectedPlan of
        [] ->
            Html.text ""

        plan :: _ ->
            div [Html.Attributes.class "rows", Html.Attributes.style  "align-items" "center"] [
            div [ Html.Attributes.class "table-container level-item" ]
                [ table [ Html.Attributes.class "table is-responsive",  Html.Attributes.style "background-color" "transparent" ]
                    [ thead []
                        [ tr []
                            [ th [Html.Attributes.class  "title is-6 has-text-white"] [Html.text "Trainingsübung" ]
                            , th [Html.Attributes.class  "title is-6 has-text-white"] [ Html.text "Sätze" ]
                            , th [Html.Attributes.class  "title is-6 has-text-white"] [ Html.text "Wdh" ] 
                            , th [Html.Attributes.class  "title is-6 has-text-white"] [ Html.text "Gewicht" ] 
                            , th [Html.Attributes.class  "title is-6 has-text-white"] [ Html.text "Fortschritt"]
                            ]
                        ]
                    , tbody []
                        (List.map (\exercise -> 
                        let progress = round (progressPercentage exercise)

                        in
                            tr [Html.Attributes.style "text-align" "center"]
                            [
                              td [Html.Attributes.class  "title is-6 has-text-white"] [ Html.text exercise.name]
                            , td [Html.Attributes.class  "title is-6 has-text-white"] [ Html.text exercise.sets ]
                            , td [Html.Attributes.class  "title is-6 has-text-white"] [ Html.text exercise.reps ]
                            , 
                                case exercise.belastung of
                                    "Körpergewicht" ->
                                        td
                                                [ Html.Attributes.class "title is-6 has-text-white"
                                                ]
                                                [ Html.text ("Body") ]
                                    _ ->
                                        let
                                            exerciseIndex =
                                                List.indexedMap (\idx ex -> (idx, ex)) plan.exercises
                                                |> List.filter (\(_, ex) -> ex == exercise)
                                                |> List.head
                                                |> Maybe.map Tuple.first
                                                |> Maybe.withDefault (-1)
                                        in
                                        if exercise.weight_now > 0 then
                                            td
                                                [ Html.Attributes.class "title is-6 has-text-white hbb2"
                                                , onClick <| OpenModal <| OpenChangeLoadExercise plan exercise exerciseIndex
                                                ]
                                                [ Html.text (String.fromFloat exercise.weight_now ++ " kg") ]
                                        else
                                            td
                                                []
                                                [ i
                                                    [ Html.Attributes.class "fa fa-plus button is-ghost hbb2"
                                                    , Html.Attributes.attribute "aria-hidden" "true"
                                                    , onClick <| OpenModal <| OpenChangeLoadExercise plan exercise exerciseIndex
                                                    ]
                                                    []
                                                ]

                            ,       
                            case exercise.belastung of
                                "Körpergewicht" ->
                                     td [][ Html.text "" ]

                                _ ->
                                    if exercise.start_weight >0 then
                                    td 
                                            [ Html.Attributes.class "pzoom"
                                              
                                            ] 
                                            [progressBar exercise]
                                    else 
                                    td [Html.Attributes.class "title is-5 has-text-white pzoom" , Html.Attributes.classList [ ( "animate__animated animate__zoomIn", True ) ]] [ Html.text "No Progress" ]

                                 
                            ]) plan.exercises)
                    ]
                ]
             , div [Html.Attributes.class "level-item"] [button [Html.Attributes.class "button is-danger is-responsive", onClick <| OpenModal <| OpenDeletePlan plan.id][Html.text "Trainingsplan löschen"]]]

inputWorkoutPlanView : Model -> Html Msg
inputWorkoutPlanView model =
    case model.newPlan of
        Nothing ->
            Html.text ""

        Just workoutPlan ->
            div [ ]
                [ div [Html.Attributes.class "level-item row"]  [
                    input [ Html.Attributes.class "input", placeholder "Name",onInput (\title -> UpdateInput { workoutPlan | title = title }) , Html.Attributes.style "margin-right" "5px"] []
                , input [ Html.Attributes.class "input", placeholder "Wochentag", onInput (\weekday -> UpdateInput { workoutPlan | weekday = weekday }) , Html.Attributes.style "margin-left" "5px"] []]
                ,div [] (List.indexedMap (exerciseInput model workoutPlan) workoutPlan.exercises)
                ,div [Html.Attributes.class "level-item", Html.Attributes.style "margin-top" "10px"] [
                 button [ Html.Attributes.class "button is-info is-responsive", onClick (AddExercise workoutPlan), Html.Attributes.style "margin-right" "10px" ] [ Html.text "Übung hinzufügen" ]
                , button [ Html.Attributes.class "button is-success is-responsive", onClick (SaveInput workoutPlan), Html.Attributes.style "margin-left" "10px" ] [ Html.text "Trainingsplan sichern" ]]
                ]

exerciseInput : Model -> WorkoutPlan -> Int -> Exercise -> Html Msg
exerciseInput model workoutPlan index exercise =
    let
        updateExercise trainingName sets reps =
            let
                maybeTraining = List.filter (\t -> t.name == trainingName) model.exercises.trainings |> List.head
                belastung = case maybeTraining of
                                Just training -> training.belastung
                                Nothing -> ""

                newExercise = { name = trainingName, sets = sets, reps = reps, belastung = belastung, start_weight = 0, weight_now = 0 }
                updatedExercises = List.indexedMap (\i ex -> if i == index then newExercise else ex) workoutPlan.exercises
                
            in
            UpdateInput { workoutPlan | exercises = updatedExercises }
    in
    div [ Html.Attributes.class "level-item",Html.Attributes.style "margin-top" "5px"]
        [ div [Html.Attributes.class "select is-info title is-6"] [select [Html.Attributes.class "is-focused", onInput (\name -> updateExercise name exercise.sets exercise.reps) ,Html.Attributes.style "margin-right" "5px", Html.Attributes.style "width" "30vw"]  (trainingOptions model.exercises.trainings)]
        , input [ Html.Attributes.class "input",placeholder "Sets", onInput (\sets -> updateExercise exercise.name sets exercise.reps),Html.Attributes.style "width" "20%" ,Html.Attributes.style "margin-right" "5px" ] []
        , input [ Html.Attributes.class "input",placeholder "Reps", onInput (\reps -> updateExercise exercise.name exercise.sets reps),Html.Attributes.style "width" "20%",Html.Attributes.style "margin-right" "5px" ] []
        , button [Html.Attributes.class "button is-danger is-responsive", onClick (RemoveExercise workoutPlan index) ] [ Html.text " Entfernen" ]
        ]

trainingOptions : List Training -> List (Html Msg)
trainingOptions trainings =
    option [ value "", selected True, disabled True ] [ Html.text "Übung" ] :: List.map (\training -> option [ value training.name ] [ Html.text training.name ]) trainings

progressPercentage : Exercise -> Float
progressPercentage exercise =
        ( ( exercise.weight_now /  exercise.start_weight) - 1 )  * 100


progressBar : Exercise -> Svg.Svg msg
progressBar exercise =
    let
        progress = round (progressPercentage exercise)
        progressWidth = String.fromInt ( progress) ++ "%"

    in
    Svg.svg 
        [ Svg.Attributes.viewBox "0 0 120 10"
        , Svg.Attributes.width "150px"
        , Svg.Attributes.class "animate__animated animate__zoomIn"
        ] 
        [ 
            
        Svg.rect 
            [ Svg.Attributes.x "0"
            , Svg.Attributes.y "0"
            , Svg.Attributes.width 
                (if progress >= 100 then 
                    "75" 
                else 
                    String.fromFloat (toFloat progress * 0.75)
                )
            , Svg.Attributes.height "100%"
            , Svg.Attributes.fill
                (if progress >= 100 then "#006400"
                else if progress >= 50 then "#48c78e"
                else "#fad02c"
                )
            , Svg.Attributes.rx "3"
            ] 
            []
           , Svg.text_ 
            [ Svg.Attributes.x "100"
            , Svg.Attributes.y "6"
            , Svg.Attributes.fill "white"
            , Svg.Attributes.fontSize "12"
            , Svg.Attributes.textAnchor "middle"
            , Svg.Attributes.dominantBaseline "middle" 
            , Svg.Attributes.fontWeight "bold"
            ] 
            [ Svg.text progressWidth ]
        
        ]

modalView : Model -> Html Msg
modalView model =
    case model.modal of
        Nothing ->
            Html.text ""



        Just modalState ->
            div []
                [case modalState of
                    DelPlan int ->
                         div [ Html.Attributes.class "modal is-active"]
                            [ div [ Html.Attributes.class "modal-background" ,onClick (CloseModal)]
                                []
                            , div 
                                        [ Html.Attributes.class "modal-card"   
                                        , Html.Attributes.style "border" "none"
                                        , Html.Attributes.style "margin" "15px auto"
                                        , Html.Attributes.style "max-width" "350px"
                                        , Html.Attributes.style "border-radius" "6px"
                                        , Html.Attributes.style "box-shadow" "0 3px 7px rgba(0, 0, 0, 0.3)"
                                        , Html.Attributes.classList [ ( "animate__animated animate__zoomIn", True )  ] 
                                        ]
                                        [ div [ Html.Attributes.class "modal-card-head custom-modal-header is-flex is-justify-content-space-between" ]
                                            [ p [] [ span [ Html.Attributes.class "title is-4 has-text-black" ] [ Html.text "Löschen" ] ] ] 

                                        , div [ Html.Attributes.class "modal-card-body custom-modal-body" ]
                                            [ p [ Html.Attributes.style "margin-bottom" "20px" ]
                                                [ span [ Html.Attributes.class "title is-5 has-text-black" ] [ Html.text "Diesen Trainingsplan entfernen" ]
                                                ]
                                            , button [ Html.Attributes.class "button is-danger ", Html.Attributes.style "margin" "10px", Html.Attributes.style "text-align" "center", onClick (DeleteWorkoutPlan int) ] [ Html.text "Ja" ]
                                            , button [ Html.Attributes.class "button is-success ", Html.Attributes.style "margin" "10px", Html.Attributes.style "text-align" "center", onClick CloseModal ] [ Html.text "Nein" ]
                                            ]

                                            ]]

                    ChangeLoad workoutplan exercise ->
                         div [ Html.Attributes.class "modal is-active"]
                            [ div [ Html.Attributes.class "modal-background" ,onClick (CloseModal)]
                                []
                            , div 
                                        [ Html.Attributes.class "modal-card"   
                                        , Html.Attributes.style "border" "none"
                                        , Html.Attributes.style "margin" "15px auto"
                                        , Html.Attributes.style "max-width" "350px"
                                        , Html.Attributes.style "border-radius" "6px"
                                        , Html.Attributes.style "box-shadow" "0 3px 7px rgba(0, 0, 0, 0.3)"
                                        , Html.Attributes.classList [ ( "animate__animated animate__zoomIn", True )  ] 
                                        ]
                                        [ div [ Html.Attributes.class "modal-card-head custom-modal-header is-flex is-justify-content-space-between" ]
                                            [ p [] [ span [ Html.Attributes.class "title is-4 has-text-black" ] [ Html.text "Gewicht aktualisieren" ] ] ] 

                                        , div [ Html.Attributes.class "modal-card-body custom-modal-body" ]
                                            [ input [ Html.Attributes.class "input", placeholder "Arbeitsgewicht in kg", onInput (UpdateNewWeightString) , Html.Attributes.style "margin-bottom" "10px" ] []
                                            , button [ Html.Attributes.class "button is-success", Html.Attributes.style "margin" "10px", Html.Attributes.style "text-align" "center", onClick (UpdateExerciseWeight) ] [ Html.text "Speichern" ]
                                            , button [ Html.Attributes.class "button is-danger", Html.Attributes.style "margin" "10px", Html.Attributes.style "text-align" "center", onClick CloseModal ] [ Html.text "Abbrechen" ]
                                            ]

                                            ]]


                    ]
