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



type alias Model =
    { firebase : Firebase.Model 
    , exercises : Exercises.Model
    , trainings : List WorkoutPlan
    , selectedPlanId : Maybe Int
    , dropdownOpen : Bool
    , newPlan : Maybe WorkoutPlan}

init : ( Model, Cmd Msg )
init =
    let
        ( exercisesModel, exercisesCmd ) = Exercises.init
    in
    ( {  firebase = Firebase.init
       , exercises = exercisesModel
       , trainings = []
       , selectedPlanId = Nothing
       , dropdownOpen = False
       , newPlan = Nothing
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
            ( model, Firebase.deleteWorkoutPlan idString )

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
        [ button [ Html.Attributes.class "button is-success", Html.Attributes.style "margin" "10px", onClick OpenInput ]
            [ Html.text "Add Workoutplan" ]  
            
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
                [ button [ Html.Attributes.class "button is-success"
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
                [ div [ Html.Attributes.class "dropdown-content" ] (List.concatMap (\plan -> [button [Html.Attributes.class "button is-white dropdown-item", onClick (SelectWorkoutPlan plan.id)] [Html.text (plan.title ++ " : " ++ plan.weekday)]]) model.trainings) ]
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
                [ table [ Html.Attributes.class "table",  Html.Attributes.style "background-color" "transparent" ]
                    [ thead []
                        [ tr []
                            [ th [Html.Attributes.class  "title is-5 has-text-white"] [Html.text "Übung" ]
                            , th [Html.Attributes.class  "title is-5 has-text-white"] [ Html.text "Sets" ]
                            , th [Html.Attributes.class  "title is-5 has-text-white"] [ Html.text "Reps" ] 
                            , th [Html.Attributes.class  "title is-5 has-text-white"] [ Html.text "Load" ] 
                            , th [Html.Attributes.class  "title is-5 has-text-white"] [ Html.text "Progress"]
                            ]
                        ]
                    , tbody []
                        (List.map (\exercise -> 
                        let progress = round (progressPercentage exercise)

                        in
                            tr [Html.Attributes.style "text-align" "center"]
                            [
                              td [Html.Attributes.class  "title is-5 has-text-white"] [ Html.text exercise.name]
                            , td [Html.Attributes.class  "title is-5 has-text-white"] [ Html.text exercise.sets ]
                            , td [Html.Attributes.class  "title is-5 has-text-white"] [ Html.text exercise.reps ]
                            , 
                            case exercise.belastung of
                                "Körpergewicht" ->
                                                    td 
                                                                [ Html.Attributes.style "color" "#6a5acd"
                                                               
                                                                ] 
                                                                [ Html.img 
                                                                    [ Html.Attributes.src "./push-up-svgrepo-com.svg", Html.Attributes.style "margin" "-10px", Html.Attributes.style "width" "30px"
                                                                    ] 
                                                                    []
                                                                ]
                                _ ->
                                        if exercise.weight_now > 0 then
                                            td [Html.Attributes.class  "title is-5 has-text-white"] [ Html.text (String.fromFloat (exercise.weight_now) ++ "kg" ) ] -- button zum ändern des gewichts hinterlegen (modal) --onclick openModalExercise plan plan.exercise (übergabe von plan und exercise da plan anschließend überschrieben wird) - eingabe feld, speichern, abbrechen if bedingung start weight = empty
                            
                                        
                                        else 
                                            td  [ ]
                                                    [   i [ Html.Attributes.class "fa fa-plus button is-ghost" , Html.Attributes.attribute "aria-hidden" "true" ] [] ]
                            
                            ,           
                            case exercise.belastung of
                                "Körpergewicht" ->
                                     td [Html.Attributes.style "color" "#6a5acd" , Html.Attributes.classList [ ( "animate__animated animate__zoomIn", True ) ]] [ Html.text "Bodyweight" ]


                                _ ->
                                    if exercise.start_weight >0 then
                                    td 
                                            [ Html.Attributes.style "display" "flex"
                                            , Html.Attributes.style "justify-content" "center" 
                                            , Html.Attributes.style "align-items" "center" 
                                             , Html.Attributes.classList [ ( "animate__animated animate__zoomIn", True ) ]
                                            ] 
                                            [progressBar exercise]
                                    else 
                                    td [Html.Attributes.style "color" "#6a5acd" , Html.Attributes.classList [ ( "animate__animated animate__zoomIn", True ) ]] [ Html.text "No Progress" ]
                            
                                 
                            ]) plan.exercises)
                    ]
                ]
             , div [Html.Attributes.class "level-item"] [button [Html.Attributes.class "button is-danger", onClick (DeleteWorkoutPlan plan.id)][Html.text "Trainingsplan löschen"]]] -- modal -bist du sicher? ja nein openModalDeletePlan plan.id button ja -> DeleteworkoutPlan plan.id ; button nein -> OpenModal = false

inputWorkoutPlanView : Model -> Html Msg
inputWorkoutPlanView model =
    case model.newPlan of
        Nothing ->
            Html.text ""

        Just workoutPlan ->
            div [ ]
                [ div [Html.Attributes.class "level-item row"]  [
                    input [ Html.Attributes.class "input", placeholder "Title",onInput (\title -> UpdateInput { workoutPlan | title = title }) , Html.Attributes.style "margin-right" "5px"] []
                , input [ Html.Attributes.class "input", placeholder "Weekday", onInput (\weekday -> UpdateInput { workoutPlan | weekday = weekday }) , Html.Attributes.style "margin-left" "5px"] []]
                ,div [] (List.indexedMap (exerciseInput model workoutPlan) workoutPlan.exercises)
                ,div [Html.Attributes.class "level-item", Html.Attributes.style "margin-top" "10px"] [
                 button [ Html.Attributes.class "button is-info", onClick (AddExercise workoutPlan), Html.Attributes.style "margin-right" "10px" ] [ Html.text "Übung hinzufügen" ]
                , button [ Html.Attributes.class "button is-info", onClick (SaveInput workoutPlan), Html.Attributes.style "margin-left" "10px" ] [ Html.text "Trainingsplan sichern" ]]
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
        [ div [Html.Attributes.class "select is-info"] [select [Html.Attributes.class "is-focused", onInput (\name -> updateExercise name exercise.sets exercise.reps) ,Html.Attributes.style "margin-right" "5px"] (trainingOptions model.exercises.trainings)]
        , input [ Html.Attributes.class "input",placeholder "Sets", onInput (\sets -> updateExercise exercise.name sets exercise.reps),Html.Attributes.style "width" "20%" ,Html.Attributes.style "margin-right" "5px" ] []
        , input [ Html.Attributes.class "input",placeholder "Reps", onInput (\reps -> updateExercise exercise.name exercise.sets reps),Html.Attributes.style "width" "20%",Html.Attributes.style "margin-right" "5px" ] []
        , button [Html.Attributes.class "button is-danger", onClick (RemoveExercise workoutPlan index) ] [ Html.text "Entfernen" ]
        ]

trainingOptions : List Training -> List (Html Msg)
trainingOptions trainings =
    option [ value "", selected True, disabled True ] [ Html.text "Übung auswählen" ] :: List.map (\training -> option [ value training.name ] [ Html.text training.name ]) trainings

progressPercentage : Exercise -> Float
progressPercentage exercise =
        ( ( exercise.weight_now /  exercise.start_weight) - 1 )  * 100


progressBar : Exercise -> Svg.Svg msg
progressBar exercise =
    let
        progress = round (progressPercentage exercise)
        progressWidth = String.fromInt ( progress // 1  ) ++ "%"

    in
        if progress >= 100 then
            Svg.svg [ Svg.Attributes.viewBox "0 0 150 25", Svg.Attributes.width "150px" ]
                [ Svg.rect [ Svg.Attributes.x "0", Svg.Attributes.y "0", Svg.Attributes.width "100%", Svg.Attributes.height "100%", Svg.Attributes.fill "#6a5acd", Svg.Attributes.rx "10", Svg.Attributes.ry "10" ] []
                , Svg.text_ [ Svg.Attributes.x "75", Svg.Attributes.y "12", Svg.Attributes.fill "#ffd500", Svg.Attributes.fontSize "14", Svg.Attributes.textAnchor "middle", Svg.Attributes.dominantBaseline "middle" ] [ Svg.text (String.fromInt progress ++ "%") ]
                ]

        

        else
                        Svg.svg [ Svg.Attributes.viewBox "0 0 150 25", Svg.Attributes.width "150px" ]
                    [ Svg.rect [ Svg.Attributes.x "0", Svg.Attributes.y "0", Svg.Attributes.width progressWidth, Svg.Attributes.height "100%", Svg.Attributes.fill "#6a5acd", Svg.Attributes.rx "10", Svg.Attributes.ry "10" ] []
                    , Svg.text_ [ Svg.Attributes.x (String.fromInt (round (toFloat progress * 0.75))) , Svg.Attributes.y "12", Svg.Attributes.fill "#ffd500", Svg.Attributes.fontSize "14", Svg.Attributes.textAnchor "middle", Svg.Attributes.dominantBaseline "middle" ] [ Svg.text (String.fromInt progress ++ "%") ]
                    ]
            
