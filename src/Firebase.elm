port module Firebase exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode
import Json.Decode.Pipeline
import Json.Encode

port signIn : () -> Cmd msg


port signInInfo : (Json.Encode.Value -> msg) -> Sub msg


port signInError : (Json.Encode.Value -> msg) -> Sub msg


port signOut : () -> Cmd msg


port saveWorkoutPlan : Json.Encode.Value -> Cmd msg


port receiveWorkoutPlans : (Json.Encode.Value -> msg) -> Sub msg

port deleteWorkoutPlan : String -> Cmd msg


type alias ErrorData =
    { code : Maybe String, message : Maybe String, credential : Maybe String }


type alias UserData =
    { token : String, email : String, uid : String }


type alias Model = 
    { userData : Maybe UserData
    , error : ErrorData}

init : Model
init =
    { userData = Maybe.Nothing
    , error = emptyError
    }


type Msg
    = LogIn
    | LogOut
    | LoggedInData (Result Json.Decode.Error UserData)
    | LoggedInError (Result Json.Decode.Error ErrorData)

emptyError : ErrorData
emptyError =
    { code = Maybe.Nothing, credential = Maybe.Nothing, message = Maybe.Nothing }    


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LogIn ->
            ( model, signIn () )

        LogOut ->
            ( { model | userData = Maybe.Nothing, error = emptyError }, signOut () )

        LoggedInData result ->
            case result of
                Ok value ->
                    ( { model | userData = Just value }, Cmd.none )

                Err error ->
                    ( { model | error = messageToError <| Json.Decode.errorToString error }, Cmd.none )

        LoggedInError result ->
            case result of
                Ok value ->
                    ( { model | error = value }, Cmd.none )

                Err error ->
                    ( { model | error = messageToError <| Json.Decode.errorToString error }, Cmd.none )

messageToError : String -> ErrorData
messageToError message =
    { code = Maybe.Nothing, credential = Maybe.Nothing, message = Just message }


errorPrinter : ErrorData -> String
errorPrinter errorData =
    Maybe.withDefault "" errorData.code ++ " " ++ Maybe.withDefault "" errorData.credential ++ " " ++ Maybe.withDefault "" errorData.message


userDataDecoder : Json.Decode.Decoder UserData
userDataDecoder =
    Json.Decode.succeed UserData
        |> Json.Decode.Pipeline.required "token" Json.Decode.string
        |> Json.Decode.Pipeline.required "email" Json.Decode.string
        |> Json.Decode.Pipeline.required "uid" Json.Decode.string


logInErrorDecoder : Json.Decode.Decoder ErrorData
logInErrorDecoder =
    Json.Decode.succeed ErrorData
        |> Json.Decode.Pipeline.required "code" (Json.Decode.nullable Json.Decode.string)
        |> Json.Decode.Pipeline.required "message" (Json.Decode.nullable Json.Decode.string)
        |> Json.Decode.Pipeline.required "credential" (Json.Decode.nullable Json.Decode.string)

loginView: Model -> Html Msg
loginView model =

    case model.userData of
                    Maybe.Nothing ->
                        div [style "display" "flex", style "flex-direction" "column", style "justify-content" "center", style "align-items" "center"] [
                        h1 [class "title is-2 has-text-black "] [ text "GymJournal" ]   
                        ,button [ class "button is-dark", style "width" "150px", onClick LogIn] [ text "Login" ]
                        ]

                    Just data ->
                        div [style "display" "flex", style "flex-direction" "column", style "justify-content" "center", style "align-items" "center"] [
                         h1 [class "title"] [ text data.email   ]
                         , button [ class "button is-dark", style "width" "150px", onClick LogOut] [ text "Logout" ]
                        , h2 [] [ text <| errorPrinter model.error ]
                        ]


                
