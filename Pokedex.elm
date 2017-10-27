module Pokedex exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Types exposing (..)


pokedexHtml : Model -> Html msg
pokedexHtml model =
    div [ id "pokedex", class "center" ]
        [ div [ id "left" ]
            [ div [ id "logo" ]
                []
            , div [ id "bg_curve1_left" ]
                []
            , div [ id "bg_curve2_left" ]
                []
            , div [ id "curve1_left" ]
                [ div [ id "buttonGlass" ]
                    [ div [ id "reflect" ]
                        []
                    ]
                , div [ id "miniButtonGlass1" ]
                    []
                , div [ id "miniButtonGlass2" ]
                    []
                , div [ id "miniButtonGlass3" ]
                    []
                ]
            , div [ id "curve2_left" ]
                [ div [ id "junction" ]
                    [ div [ id "junction1" ]
                        []
                    , div [ id "junction2" ]
                        []
                    ]
                ]
            , div [ id "screen" ]
                [ div [ id "topPicture" ]
                    [ div [ id "buttontopPicture1" ]
                        []
                    , div [ id "buttontopPicture2" ]
                        []
                    ]
                , div [ id "picture" ]
                    [ img [ alt "psykokwak", attribute "height" "170", src model.pokeData.pokeImg ]
                        []
                    , text "      "
                    ]
                , div [ id "buttonbottomPicture" ]
                    []
                , div [ id "speakers" ]
                    [ div [ class "sp" ]
                        []
                    , div [ class "sp" ]
                        []
                    , div [ class "sp" ]
                        []
                    , div [ class "sp" ]
                        []
                    ]
                ]
            , div [ id "bigbluebutton" ]
                []
            , div [ id "barbutton1" ]
                []
            , div [ id "barbutton2" ]
                []
            , div [ id "cross" ]
                [ div [ id "leftcross" ]
                    [ div [ id "leftT" ]
                        []
                    ]
                , div [ id "topcross" ]
                    [ div [ id "upT" ]
                        []
                    ]
                , div [ id "rightcross" ]
                    [ div [ id "rightT" ]
                        []
                    ]
                , div [ id "midcross" ]
                    [ div [ id "midCircle" ]
                        []
                    ]
                , div [ id "botcross" ]
                    [ div [ id "downT" ]
                        []
                    ]
                ]
            ]
        , div [ id "right" ]
            [ p [ id "stats", class "f4" ]
                [ strong [ class "pokeFont f5" ]
                    [ text "Name : " ]
                , text model.searchTerm
                , br [] []
                , strong [ class "pokeFont f5" ]
                    [ text "Type : " ]
                , text <| String.join ", " model.pokeData.types
                , br [] []
                , strong [ class "pokeFont f5" ]
                    [ text "Height : " ]
                , text (toString model.pokeData.height_ ++ "''")
                , br [] []
                , strong [ class "pokeFont f5" ]
                    [ text "Weight : " ]
                , text (toString model.pokeData.weight ++ " lbs.")
                , br [] []
                , strong [ class "pokeFont f5" ]
                    [ text "Abilities" ]
                , br [] []
                , text <| String.join ", " model.pokeData.abilities
                ]
            , div [ id "miniButtonGlass4" ]
                []
            , div [ id "miniButtonGlass5" ]
                []
            , div [ id "barbutton3" ]
                []
            , div [ id "barbutton4" ]
                []
            , div [ id "yellowBox1" ]
                []
            , div [ id "yellowBox2" ]
                []
            , div [ id "bg_curve1_right" ]
                []
            , div [ id "bg_curve2_right" ]
                []
            , div [ id "curve1_right" ]
                []
            , div [ id "curve2_right" ]
                []
            ]
        ]