module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)


main : Program Never Model Msg
main =
    Html.program { init = init, view = view, subscriptions = always Sub.none, update = update }


type alias Model =
    { searchTerm : String }


model : Model
model =
    { searchTerm = "" }


init : ( Model, Cmd Msg )
init =
    ( model, Cmd.none )


type Msg
    = TextInput String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TextInput input ->
            ( { model | searchTerm = input }, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ input [] []
        , ul [] []
        ]
