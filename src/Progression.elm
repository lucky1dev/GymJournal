module Progression exposing (..)

import Html exposing (Html, button, div, text)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Svg exposing (..)
import Svg.Attributes as SvgA exposing (fill)
import Process exposing (sleep)
import Task

type alias Exercise =
    { name : String
    , startWeight : Float
    , currentWeight : Float
    }

type alias Model =
    { exercises : List Exercise
    , percentage : Float
    , isAnimating : Bool
    }

init : Model
init  =
    { exercises = 
        [ { name = "Bankdrücken"
          , startWeight = 50 
          , currentWeight = 50 
          } ]
    , percentage = 0 
    , isAnimating = False 
    }

type Msg
    = ToggleAnimation
    | IncreaseWeight

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ToggleAnimation ->
            let
                newModel = { model | isAnimating = not model.isAnimating }
            in
            if newModel.isAnimating then
                ( newModel, Task.perform (always IncreaseWeight) (Process.sleep 100) )
            else
                ( newModel, Cmd.none )

        IncreaseWeight ->
            let
                exercise = List.head model.exercises |> Maybe.withDefault { name = "", startWeight = 0, currentWeight = 0 }
                newWeight = exercise.currentWeight + 1
                newPercentage = ((newWeight - exercise.startWeight) / exercise.startWeight) * 100
                updatedExercise = { exercise | currentWeight = newWeight }
            in
            if newWeight > exercise.startWeight + exercise.startWeight then
                ( model, Cmd.none )
            else
                ( { model | exercises = [updatedExercise], percentage = newPercentage }, Task.perform (always IncreaseWeight) (Process.sleep 100) )

view : Model -> Html Msg
view model =
    let
        exercise = List.head model.exercises |> Maybe.withDefault { name = "", startWeight = 0, currentWeight = 0 }
    in
    div [ Html.Attributes.style "display" "flex", Html.Attributes.style "justify-content" "center", Html.Attributes.style "align-items" "center", Html.Attributes.style "height" "100vh", Html.Attributes.style "flex-direction" "column" ]
          [ div [] [ Html.text exercise.name ]
          , button [class "button is-danger", onClick ToggleAnimation ] [ Html.text "Toggle Animation" ]
          , svg [ SvgA.viewBox "0 0 120 100", SvgA.width "240px", SvgA.height "70%" ]
                [ g [ SvgA.transform "translate(10, 10)"]
                    [ 
                     rect [ SvgA.x "0", SvgA.y "50", SvgA.height "15", SvgA.width (String.fromFloat model.percentage), fill "cyan" ] []
                    ]
                , Svg.text_ [ SvgA.x "110", SvgA.y "65", SvgA.fontSize "10", SvgA.textAnchor "middle" ] [ Svg.text (String.fromInt (round model.percentage) ++ "%") ]
                ]
            ]