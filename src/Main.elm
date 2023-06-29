module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode
import Json.Decode.Pipeline
import Json.Encode
import Planning
import Start
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
    }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
  ({  key = key
    , url = url
    , planning = Planning.init
    , firebase = Firebase.init}, Cmd.none )


---- UPDATE ----


type Msg
    = PlanningMsg Planning.Msg
    | FirebaseMsg Firebase.Msg
    | UrlChanged Url.Url
    | LinkClicked Browser.UrlRequest




update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of

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
                                    ,trainings = model.planning.trainings
                                    ,modal = model.planning.modal
                                    ,selectedPlanId = model.planning.selectedPlanId
                                    ,dropdownOpen = model.planning.dropdownOpen 
                                    ,messages = model.planning.messages}
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
       
---- VIEW ----
view : Model -> Browser.Document Msg
view model =
    { title = "GymJournal"
    , body =
        [ viewBody model
        ]
    }

viewBody : Model -> Html Msg
viewBody model =
    div [] [ navBar model,
                  case model.url.fragment of 
                        Just "exercises" -> Exercises.exercisesView
                        Just "trainings" -> div [] [Html.map PlanningMsg (Planning.planningView model.planning)]
                        Just "progression" -> Progression.progressionView
                        _ -> startView model
            ]

startView: Model -> Html Msg
startView model =

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
            ]]
            
navBar : Model -> Html Msg
navBar model =
    case model.firebase.userData of
        Maybe.Nothing ->
            text ""

        Just data ->
            nav [ class "navbar is-white", Html.Attributes.classList [ ( "animate__animated animate__fadeIn", True ) ], style "animation-delay" "3s", style "z-index" "2"]
                [ --div [ class "navbar-brand" ]
                   -- [ a [ class "navbar-item", href "" ]
                   --     [ div [ id "navbar" ]
                   --         [ h1 [ class "title" ]
                   --             [ text "GymJournal" ]
                  --          ]
                  --      ]
                  --  , a [ style "role" "button", class "navbar-burger", style "ariaLabel" "menu", style "ariaExpanded" "false", attribute "data-target" "navbarBasicExample" ]
                  --      [ span [ style "ariaHidden" "true" ] []
                  --      , span [ style "ariaHidden" "true" ] []
                  --      , span [ style "ariaHidden" "true" ] []
                  --      ]
                  --  ],
                 div [ class "navbar-menu" ]
                    [ div [ class "navbar-start" ]
                        [ a [ class "navbar-item has-text-black is-size-4 has-text-weight-bold", href "#exercises" ] [ text "Exercises" ]
                        , a [ class "navbar-item has-text-black is-size-4 has-text-weight-bold", href "#trainings" ] [ text "Trainings" ]
                        , a [ class "navbar-item has-text-black is-size-4 has-text-weight-bold", href "#progression" ] [ text "Progressions" ]
                        ]
                    , div [ class "navbar-end" ]
                        [ div [ class "navbar-item" ]
                            [ div [ class "buttons" ]
                                [ button [ class "button is-dark", onClick (Firebase.LogOut |> FirebaseMsg) ] [ strong [] [ text "Logout" ] ]
                                ]
                            ]
                        ]
                    ]
                ]


        
---- PROGRAM ----


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
