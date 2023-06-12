module Start exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)




logo : Html msg
logo =
    div [id "logo"]
        [ h1 [class "title", style "outline" "none"]
            [ text "GymJournal"
            , div [class "aurora"]
                [ div [class "aurora__item"] []
                , div [class "aurora__item"] []
                , div [class "aurora__item"] []
                , div [class "aurora__item"] []
                ]
            ]
        ]


