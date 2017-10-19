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
    div [ class "pokeFont" ]
        [ h1 [ class "tc" ] [ text "Search for your Pokémon!" ]
        , div [ class "w-100" ]
            [ input [ class "pokeFont center db w-75 w-50-m w-33-l f3 pa2", onInput ChangeSearch ] []
            , ul [ class "pa0 center db w-75 w-50-m w-33-l f5 ma0 pa2 bg-white o-90" ] (List.map liMaker <| wordSearcher model)
            ]
        , img [ class "ash absolute", src "http://satoshipedia.altervista.org/wp-content/uploads/2015/12/ash_ketchum-467.png", alt "Ash with pokeball" ] []
        ]



-- <img class="ash" alt="Ash with pokeball" src="http://satoshipedia.altervista.org/wp-content/uploads/2015/12/ash_ketchum-467.png">


pokeRegex : String -> Regex
pokeRegex searchTerm =
    caseInsensitive <| regex <| Debug.log "regex" <| " " ++ searchTerm ++ "[a-z-]*"


wordSearcher : Model -> List String
wordSearcher model =
    if model.searchTerm == "" then
        [ "" ]
    else
        let
            editedPokeString =
                Regex.replace All (regex "\n") (\_ -> " ") pokeString
        in
            editedPokeString
                |> Regex.find (AtMost 10) (pokeRegex model.searchTerm)
                |> List.map .match


liMaker : String -> Html Msg
liMaker pokemon =
    li [ class "list boringFont" ] [ text pokemon ]
