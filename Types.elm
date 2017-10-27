module Types exposing (..)

import Http exposing (..)


type alias Model =
    { searchTerm : String
    , pokeData : PokeData
    }


type alias PokeData =
    { pokeImg : String
    , height_ : Int
    , weight : Int
    , types : List String
    , abilities : List String
    }


type Msg
    = ChangeSearch String
    | ReceivePokeData (Result Http.Error PokeData)
    | SelectPokemon String
