module Exercises exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

exercisesView : Html msg
exercisesView  =
    div [ id "my-custom-css"]  [
    div [ class "container" , Html.Attributes.classList [ ( "animate__animated animate__fadeIn", True ) ]]
        [ section [ class "section", style "background-image" "url(Muskelgruppe_Brust.jpg)" ]
            [ div [ class "content" ]
                [ p [class "has-text-white is-size-4 has-text-weight-bold"] [ text "Chest" ] ]
            , div [ class "overlay" ] []
            ]
        , section [ class "section", style "background-image" "url(Muskelgruppe_Rücken.jpg)" ]
            [ div [ class "content" ]
                [ p [class "has-text-white is-size-4 has-text-weight-bold"] [ text "Back" ] ]
            , div [ class "overlay" ] []
            ]
        , section [class "section", style "background-image" "url(Muskelgruppe_Schultern.jpg)" ]
            [ div [ class "content" ]
                [ p [class "has-text-white is-size-4 has-text-weight-bold"] [ text "Shoulders" ] ]
            , div [ class "overlay" ] []
            ]
                , section [class "section", style "background-image" "url(Muskelgruppe_Beine.jpg)" ]
            [ div [ class "content" ]
                [ p [class "has-text-white is-size-4 has-text-weight-bold"] [ text "Legs" ] ]
            , div [ class "overlay" ] []
            ]
        , section [ class "section", style "background-image" "url(Muskelgruppe_Bauch.jpg)" ]
            [ div [ class "content" ]
                [ p [class "has-text-white is-size-4 has-text-weight-bold"] [ text "Abs" ] ]
            , div [ class "overlay" ] []
            ]
        ]
    ]