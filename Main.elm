module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Regex exposing (..)
import Pokemon exposing (pokeString)


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
    = ChangeSearch String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeSearch input ->
            ( { model | searchTerm = input }, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ input [ onInput ChangeSearch ] []
        , ul [] [ text <| toString <| wordSearcher model ]
        ]


pokeRegex : String -> Regex
pokeRegex searchTerm =
    caseInsensitive <| regex <| Debug.log "regex" <| " " ++ searchTerm ++ "[a-z-]*"


wordSearcher : Model -> List Match
wordSearcher model =
    let
        editedPokeString =
            Regex.replace All (regex "\n") (\_ -> " ") pokeString
    in
        Debug.log "data" <| Regex.find (AtMost 10) (pokeRegex model.searchTerm) editedPokeString
