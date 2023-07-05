module Progression exposing (..)


import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Html.Attributes exposing (style)
import Svg exposing (..)
import Svg.Attributes as SvgA exposing (fill)
import Process exposing (sleep)
import Task

type alias Model =
    { startWeight : Float
    , currentWeight : Float
    , maxWeight : Float
    , percentage : Float
    , isAnimating : Bool
    }

init : Model
init  =
    {
         startWeight = 100 
        ,currentWeight = 0 
        ,maxWeight = 200 
        ,percentage = 0 
        , isAnimating = False }

type Msg
    = ToggleAnimation
    | IncreaseWeight

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ToggleAnimation ->
            let
                newModel =
                    { model | isAnimating = not model.isAnimating }
            in
            if newModel.isAnimating then
                ( newModel, Task.perform (always IncreaseWeight) (Process.sleep 1) )
            else
                ( newModel, Cmd.none )

        IncreaseWeight ->
            if model.currentWeight >= model.maxWeight then
                ( model, Cmd.none )
            else
                let
                    newWeight = model.currentWeight + 1
                    newPercentage = (newWeight / model.maxWeight) * 100
                in
                ( { model | currentWeight = newWeight, percentage = newPercentage }, Task.perform (always IncreaseWeight) (Process.sleep 5) )

view : Model -> Html Msg
view model =

    div [ Html.Attributes.style "display" "flex", Html.Attributes.style "justify-content" "center", Html.Attributes.style "align-items" "center", Html.Attributes.style "height" "100vh" ]
            [ svg [ SvgA.viewBox "0 0 150 150", SvgA.width "100%", SvgA.height "300px" ]
                [ g [ SvgA.transform "translate(10, 110) scale(1, -1)" ]
                    [ rect [ SvgA.x "0", SvgA.y "0", SvgA.width "40", SvgA.height (String.fromFloat (model.startWeight / model.maxWeight * 100)), fill "green" ] []
                    , rect [ SvgA.x "50", SvgA.y "0", SvgA.width "40", SvgA.height (String.fromFloat (model.currentWeight / model.maxWeight * 100)), fill "red" ] []
                    ]
                , Svg.text_ [ SvgA.x "20", SvgA.y "125", SvgA.fontSize "10", SvgA.textAnchor "middle" ] [ Svg.text (String.fromFloat model.startWeight ++ " kg") ]
                , Svg.text_ [ SvgA.x "70", SvgA.y "125", SvgA.fontSize "10", SvgA.textAnchor "middle" ] [ Svg.text (String.fromFloat model.currentWeight ++ " kg (" ++ String.fromFloat model.percentage ++ "%)") ]
                ]
            , button [ onClick ToggleAnimation ] [ Html.text "Toggle Animation" ]
            ]
        


