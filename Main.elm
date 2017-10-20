module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Regex exposing (..)
import Pokemon exposing (pokeString)
import Http exposing (..)
import Json.Decode as Json
import Json.Decode.Pipeline exposing (..)


main : Program Never Model Msg
main =
    Html.program { init = init, view = view, subscriptions = always Sub.none, update = update }


type alias Model =
    { searchTerm : String
    , pokeData : PokeData
    }


model : Model
model =
    { searchTerm = ""
    , pokeData = PokeData "" ""
    }


init : ( Model, Cmd Msg )
init =
    ( model, Cmd.none )


type Msg
    = ChangeSearch String
    | ReceivePokeData (Result Http.Error PokeData)
    | SelectPokemon String


type alias PokeData =
    { pokeImg : String, pokeImgShine : String }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeSearch input ->
            ( { model | searchTerm = String.trim input }, Cmd.none )

        ReceivePokeData (Ok data) ->
            ( { model | pokeData = data }, Cmd.none )

        ReceivePokeData (Err err) ->
            let
                log =
                    Debug.log "ERR" err
            in
                ( model, Cmd.none )

        SelectPokemon input ->
            let
                trimInput =
                    String.trim input
            in
                ( { model | searchTerm = trimInput }, pokeApiCall <| String.toLower trimInput )


view : Model -> Html Msg
view model =
    div [ class "pokeFont" ]
        [ h1 [ class "tc" ] [ text "Search for your Pokémon!" ]
        , div [ class "w-100" ]
            [ input [ class "pokeFont center db w-75 w-50-m w-33-l f3 pa2", onInput ChangeSearch, value model.searchTerm ] []
            , ul [ class "pa0 center db w-75 w-50-m w-33-l f5 ma0 pa2 bg-white o-90" ] (List.map liMaker <| wordSearcher model)
            ]
        , img [ src model.pokeData.pokeImg ] []
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
    li [ class "list boringFont blueBackground" ]
        [ button [ class "boringFont bg-inherit bn w-100 tl", onClick <| SelectPokemon pokemon ] [ text pokemon ]
        ]


pokeApiCall : String -> Cmd Msg
pokeApiCall pokeName =
    let
        url =
            "https://pokeapi.co/api/v2/pokemon/" ++ pokeName

        request =
            Http.get url pokeDecoder
    in
        Http.send ReceivePokeData request


pokeDecoder : Json.Decoder PokeData
pokeDecoder =
    decode PokeData
        |> requiredAt [ "sprites", "front_default" ] Json.string
        |> requiredAt [ "sprites", "front_shiny" ] Json.string
