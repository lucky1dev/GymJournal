module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Start
import Exercises
import Planning
import Logging
import Progression
import Url
import Browser.Navigation as Nav

---- MODEL ----


type alias Model =
    { currentView : CurrentView
    , key : Nav.Key
    , url : Url.Url

    , planning : Planning.Model
    }


type CurrentView
    = LandingPage
    | Exercises
    | Trainings
    | Logs
    | Progression

init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
  ( { currentView = LandingPage
    , key = key
    , url = url

    ,planning = { nextId = 0
    , saved = []
    , modal = Nothing
    }

    }, Cmd.none )


---- UPDATE ----


type Msg
    = SwitchToLandingPage
    | SwitchToExercises
    | SwitchToTrainings
    | SwitchToLogs
    | SwitchToProgression
    | UrlChanged Url.Url
    | LinkClicked Browser.UrlRequest

    | PlanningMsg Planning.Msg


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        SwitchToLandingPage ->
            ({ model | currentView = LandingPage }, Cmd.none)

        SwitchToExercises ->
            ({ model | currentView = Exercises }, Cmd.none)

        SwitchToTrainings ->
            ({ model | currentView = Trainings }, Cmd.none)

        SwitchToLogs ->
            ({ model | currentView = Logs }, Cmd.none)

        SwitchToProgression ->
            ({ model | currentView = Progression }, Cmd.none)
        
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


        PlanningMsg subMsg ->
            let
                (updatedPlanning, cmd) = Planning.update subMsg model.planning
            in
            ( { model | planning = updatedPlanning }, Cmd.map PlanningMsg cmd )

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

    div [] [ navBar,
                  case model.url.fragment of 
                        Just "start" -> Start.logo
                        Just "exercises" -> Exercises.exercisesView
                        Just "trainings" -> Html.map PlanningMsg (Planning.planningView model.planning)
                        Just "logs" -> Logging.loggingView
                        Just "progression" -> Progression.progressionView
                        _ -> Start.logo
            ]
        
        

navBar : Html Msg
navBar =
    nav [ class "navbar is-black", style "role" "navigation", style "ariaLabel" "main navigation", style "background-color" "black" ]
        [ div [ class "navbar-brand" ]
            [ a [ class "navbar-item", href "" ]
                [ div [ id "navbar" ]
                    [ h1 [ class "title" ]
                        [ text "GymJournal" ]
                    ]
                ]
            , a [ style "role" "button", class "navbar-burger", style "ariaLabel" "menu", style "ariaExpanded" "false", attribute "data-target" "navbarBasicExample" ]
                [ span [ style "ariaHidden" "true" ] []
                , span [ style "ariaHidden" "true" ] []
                , span [ style "ariaHidden" "true" ] []
                ]
            ]
        , div [ class "navbar-menu" ]
            [ div [ class "navbar-start" ]
                [ a [ class "navbar-item has-text-white is-size-4 has-text-weight-bold", href "#exercises"] [ text "Exercises" ]
                , a [ class "navbar-item has-text-white is-size-4 has-text-weight-bold", href "#trainings" ] [ text "Trainings" ]
                , a [ class "navbar-item has-text-white is-size-4 has-text-weight-bold", href "#logs"] [ text "Logs" ]
                , a [ class "navbar-item has-text-white is-size-4 has-text-weight-bold", href "#progression"] [ text "Progressions" ]
                ]
            , div [ class "navbar-end" ]
                [ div [ class "navbar-item" ]
                    [ div [ class "buttons" ]
                        [ a [ class "button is-info" ] [ strong [] [ text "Sign up" ] ]
                        , a [ class "button is-light" ] [ text "Log in" ]
                        ]
                    ]
                ]
            ]
        ]

main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }