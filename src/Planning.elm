module Planning exposing (..)


import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode
import Json.Decode.Pipeline
import Json.Encode
import Firebase exposing (..)


-- MODEL


type alias Model =
    { inputContent : String
    , inputDate : String
    , inputTime : String
    , firebase : Firebase.Model }

init : Model
init =
    { inputContent = ""
    , inputDate = ""
    , inputTime = ""
    , firebase = Firebase.init 
    }

type Msg
    = InputChanged String
    | DateChanged String
    | TimeChanged String
    | SaveMessage
    | FirebaseMsg Firebase.Msg


-- UPDATE

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of

        InputChanged value ->
            ( { model | inputContent = value }, Cmd.none )

        DateChanged value ->
            ( { model | inputDate = value }, Cmd.none )

        TimeChanged value ->
            ( { model | inputTime = value }, Cmd.none )

        SaveMessage ->
            ( model, Firebase.saveMessage <| messageEncoder model )

        FirebaseMsg subMsg ->
            let
                (updatedFirebase, cmd) = Firebase.update subMsg model.firebase
            in
            ( { model | firebase = updatedFirebase }, Cmd.map FirebaseMsg cmd )
        


messageEncoder : Model -> Json.Encode.Value
messageEncoder model =
    Json.Encode.object
        [ ( "content", Json.Encode.string model.inputContent )
        , ( "date", Json.Encode.string model.inputDate )
        , ( "time", Json.Encode.string model.inputTime )
        , ( "uid"
          , case model.firebase.userData of
                Just userData ->
                    Json.Encode.string userData.uid

                Maybe.Nothing ->
                    Json.Encode.null
          )
        ]


-- VIEW


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

    