module Planning exposing (..)


import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode
import Json.Decode.Pipeline
import Json.Encode
import Firebase exposing (..)


-- MODEL
type alias Exercise =
    { name : String
    , sets : String
    , reps : String
    }

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
    {  firebase : Firebase.Model 
    
    , saved : List WorkoutPlan
    , modal : Maybe ModalMsg
    , selectedPlanId : Maybe Int
    , dropdownOpen : Bool}

init : Model
init =
    {  firebase = Firebase.init 
    , saved = []
    , modal = Nothing
    , selectedPlanId = Nothing
    , dropdownOpen = False
    }

type Msg

    = FirebaseMsg Firebase.Msg
    | DeleteWorkoutPlan Int
    | OpenModal ModalMsg
    | CloseModal
    | UpdateModal ModalMsg
    | UpdateWorkoutPlan Int WorkoutPlan
    | UpdateExercise Int Int Exercise
    | SaveModal
    | AddExercise 
    | SelectWorkoutPlan Int
    | ToggleDropdown


-- UPDATE

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of

        FirebaseMsg subMsg ->
            let
                (updatedFirebase, cmd) = Firebase.update subMsg model.firebase
            in
            ( { model | firebase = updatedFirebase }, Cmd.map FirebaseMsg cmd )
        

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
                    let
                        uniqueId =
                            case   List.maximum (List.map .id model.saved) of
                                Just maxId ->
                                    maxId + 1
                                Nothing ->
                                    1
                        
                        
    
                    
                    
                    in
                    ( { model | saved = { plan | id = uniqueId } :: model.saved, modal = Nothing }, Firebase.saveWorkoutPlan <| messageEncoderWorkoutplan { plan | id = uniqueId } )

        AddExercise ->
            case model.modal of
                Just (InputWorkoutPlan workoutPlan) ->
                    let
                        updatedWorkoutPlan = { workoutPlan | exercises = workoutPlan.exercises ++ [ { name = "", sets = "", reps = "" } ] }
                    in
                    ( { model | modal = Just (InputWorkoutPlan updatedWorkoutPlan) }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ToggleDropdown ->
            ( { model | dropdownOpen = not model.dropdownOpen }, Cmd.none )

        SelectWorkoutPlan id ->
            
            ({ model | selectedPlanId = Just id, dropdownOpen = False }, Cmd.none)



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
        ]


-- VIEW

{-
firebae : Model -> Html Msg
firebae model = 
    div [] [
        case model.firebase.userData of
                    Just data ->
                        div []
                            [ input [ placeholder "Message to save", value model.inputContent, onInput InputChanged ] []
                            , input [ placeholder "Date", value model.inputDate, onInput DateChanged ] []
                            , input [ placeholder "Time", value model.inputTime, onInput TimeChanged ] []
                            , button [ onClick SaveMessage ] [ text "Save new message" ]
                            , div [ style "display" "flex", style "justify-content" "center"]
                                    [ h3 []
                                        [ text "Previous messages"
                                        , table [ class "table is-striped" ]
                                            [ thead []
                                                [ tr []
                                                    [ th [] [ text "Content" ]
                                                    , th [] [ text "Date" ]
                                                    , th [] [ text "Time" ]
                                                    ]
                                                ]
                                            , tbody []
                                                <| List.map
                                                    (\m -> tr [] [ td [] [ text m.content ], td [] [ text m.date ], td [] [ text m.time ] ])
                                                    model.firebase.messages
                                            ]
                                        ]
                                    ]
                                , h2 [] [ text <| errorPrinter model.firebase.error ]
                            ]

                    Maybe.Nothing ->
                        div [] []

    ]



-}


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
        , case model.saved of
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
                        case List.filter (\plan -> plan.id == id) model.saved of
                            [selectedPlan] ->
                                selectedPlan.title

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
                [ div [ class "dropdown-content" ] (List.concatMap (\plan -> [button [class "button is-white dropdown-item", onClick (SelectWorkoutPlan plan.id)] [text plan.title]]) model.saved) ]
            ]
        ]


mainView : Model -> Html Msg
mainView model =
    let
        selectedPlan = List.filter (\plan -> Just plan.id == model.selectedPlanId) model.saved
    in
    case selectedPlan of
        [] ->
            text ""

        plan :: _ ->
            div [ class "table-container" ]
                [ table [ class "table is-hoverable" ]
                    [ thead []
                        [ tr []
                            [ th [] [ text "Exercise" ]
                            , th [] [ text "Sets" ]
                            , th [] [ text "Reps" ]
                            ]
                        ]
                    , tbody []
                        (List.map (\exercise -> 
                            tr []
                            [ td [] [ text exercise.name ]
                            , td [] [ text exercise.sets ]
                            , td [] [ text exercise.reps ]
                            ]) plan.exercises)
                    ]
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