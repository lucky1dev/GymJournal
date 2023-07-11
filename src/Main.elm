module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode
import Json.Decode.Pipeline
import Json.Encode
import Planning
import Exercises
import Progression
import Url
import Browser.Navigation as Nav
import Firebase exposing (..) 

type alias Model =
    { key : Nav.Key
    , url : Url.Url
    , firebase : Firebase.Model
    , planning : Planning.Model
    , exercises : Exercises.Model
    , progression : Progression.Model
    }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        ( exercisesModel, exercisesCmd ) = Exercises.init
        ( planningModel, planningCmd ) = Planning.init
    in
    ( { key = key
      , url = url
      , planning = planningModel
      , firebase = Firebase.init
      , exercises = exercisesModel
      , progression = Progression.init
      }
    , Cmd.batch [ Cmd.map ExercisesMsg exercisesCmd
                , Cmd.map PlanningMsg planningCmd
                ]
    )


type Msg
    = PlanningMsg Planning.Msg
    | ProgressionMsg Progression.Msg
    | FirebaseMsg Firebase.Msg
    | ExercisesMsg Exercises.Msg
    | UrlChanged Url.Url
    | LinkClicked Browser.UrlRequest

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        
        ProgressionMsg subMsg ->
            let
                (updatedProgression, cmd) = Progression.update subMsg model.progression
            in
            ( { model | progression = updatedProgression }, Cmd.map ProgressionMsg cmd )
    

        ExercisesMsg subMsg ->
            let
                (updatedExercises, cmd) = Exercises.update subMsg model.exercises
            in
            ( { model | exercises = updatedExercises }, Cmd.map ExercisesMsg cmd )

        PlanningMsg subMsg ->
            let
                (updatedPlanning, cmd) = Planning.update subMsg model.planning
            in
            ( { model | planning = updatedPlanning }, Cmd.map PlanningMsg cmd )

        FirebaseMsg subMsg ->
            let
                (updatedFirebase, cmd) = Firebase.update subMsg model.firebase
            in
            ( { model | firebase = updatedFirebase
                     , planning = { firebase = updatedFirebase 
                                    ,exercises = model.planning.exercises
                                    ,trainings = model.planning.trainings
                                    ,selectedPlanId = model.planning.selectedPlanId
                                    ,dropdownOpen = model.planning.dropdownOpen
                                    ,newPlan = model.planning.newPlan }
               }
            , Cmd.map FirebaseMsg cmd )

        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )
                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            case url.fragment of 
                Nothing -> ( { model | url = {url | fragment=Just "start"}} 
                    , Cmd.none
                    )
                _ -> ( { model | url = url }
                    , Cmd.none
                    )
       
view : Model -> Browser.Document Msg
view model =

    let

        container =
            div [ id "container",  style "position" "absolute" ,style "z-index" "-2" ]
                [ div [ id "container-inside", style "position" "relative" ,style "z-index" "-1" ]
                    [ div [ id "circle-small" ] []
                    , div [ id "circle-medium" ] []
                    , div [ id "circle-large" ] []
                    , div [ id "circle-xlarge" ] []
                    , div [ id "circle-xxlarge" ] []
                    ]
                ]

             
    in
        { title = "GymJournal"
        , body =
            [ container, viewBody model ]
        }

viewBody : Model -> Html Msg
viewBody model =
                  div [style "position" "absolute" ,style "z-index" "5",style "min-height" "100%", style "min-width" "100%" ] [ navBar model,
                        case model.url.fragment of 
                                Just "exercises" -> div [] [Html.map ExercisesMsg (Exercises.exercisesView model.exercises)]
                                Just "trainings" -> div [] [Html.map PlanningMsg (Planning.planningView model.planning)]
                                Just "progression" ->  div [] [Html.map ProgressionMsg (Progression.view model.progression)]
                                _ -> startView model
                    ]


startView: Model -> Html Msg
startView model =

     div [] [

          case model.firebase.userData of
                Maybe.Nothing ->
                        video [ attribute "autoplay" "true", attribute "muted" "true", attribute "loop" "true", id "myVideo", style "opacity" "0.9" ]
                                [ source [ src "./sport2.mp4", type_ "video/mp4" ] []
                                ]


                Just data -> 
                    text ""



    , div [style "display" "flex"
        , style "justify-content" "center"
        , style "align-items" "center"
        , style "height" "100vh"] [div 
            [ style "position" "relative"
            , style "z-index" "1"
            ]
            [
    div [] [Html.map FirebaseMsg (Firebase.loginView model.firebase)]]]]
    {-
    div [ class "welcome", style "z-index" "1"]
        [ span [ id "splash-overlay", class "splash" ] []
        , span [ id "welcome", class "z-depth-4" ] []
        ,  div
            [ classList
                [ ( "animate__animated animate__fadeIn", True )
                ]
            , style "animation-delay" "3s"
            , style "display" "flex"
            , style "flex-direction" "column"  
            , style "align-items" "center"
            , style "justify-content" "center"
            , style "padding-top" "30vh"
            ]
            [ 

                    Html.map FirebaseMsg (Firebase.loginView model.firebase)
            ]] -}
            
navBar : Model -> Html Msg --animations
navBar model =
  case model.firebase.userData of
        Maybe.Nothing ->
            text ""

        Just data -> 

            div [ style "display" "flex", style "flex-direction" "row", style "justify-content" "center", style "align-items" "center",  style "position" "relative"
                    , style "z-index" "2"] 
            [ 
                
                 div [style "width" "33vw", style "display" "flex", style "justify-content" "center", style "align-items" "center"] [button [class "button is-ghost"] [ a [ class "title is-5 has-text-white", href "#exercises" ] [ text "Exercises" ]]]
                , div [  style "width" "33vw", style "display" "flex", style "justify-content" "center", style "align-items" "center"][ button [class "button is-ghost"] [ a [ class "title is-5 has-text-white",  href "#trainings" ] [ text "Trainings" ] ] ]
                , div [  style "width" "33vw", style "display" "flex", style "justify-content" "center", style "align-items" "center"] [ button [class "button is-ghost"] [ a [ class "title is-5 has-text-white", href ""] [text "Account"] ]  ]
            ]

                


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Firebase.signInInfo (Json.Decode.decodeValue Firebase.userDataDecoder) |> Sub.map (FirebaseMsg << Firebase.LoggedInData)
        , Firebase.signInError (Json.Decode.decodeValue Firebase.logInErrorDecoder) |> Sub.map (FirebaseMsg << Firebase.LoggedInError)
        , Firebase.receiveWorkoutPlans (Json.Decode.decodeValue Planning.workoutListDecoder) |> Sub.map (PlanningMsg << Planning.WorkoutPlansReceived)
        ]


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }
