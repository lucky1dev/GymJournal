module Start exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)




logo : Html msg
logo =
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
            [ h1 [class "title"] [ text "GymJournal" ]
            , button [ class "button is-dark", style "width" "150px"] [ text "Login" ]
            ]]
        