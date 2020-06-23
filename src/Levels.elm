module Levels exposing (getLevelInfo, viewLevels)

import Dict
import Html exposing (Html, div, h3, table, td, text, th, tr)


type alias LevelInfo =
    { hp : Int, buildChances : List Int }


levelInfoDict =
    Dict.fromList
        [ ( 1, { hp = 10, buildChances = [ 100, 0, 0 ] } )
        , ( 2, { hp = 50, buildChances = [ 100, 0, 0 ] } )
        , ( 3, { hp = 100, buildChances = [ 100, 0, 0 ] } )
        , ( 4, { hp = 500, buildChances = [ 100, 0, 0 ] } )
        , ( 5, { hp = 1000, buildChances = [ 100, 0, 0 ] } )
        , ( 6, { hp = 5000, buildChances = [ 100, 0, 0 ] } )
        , ( 7, { hp = 10, buildChances = [ 100, 0, 0 ] } )
        , ( 8, { hp = 10, buildChances = [ 100, 0, 0 ] } )
        , ( 9, { hp = 10, buildChances = [ 100, 0, 0 ] } )
        , ( 10, { hp = 10, buildChances = [ 100, 0, 0 ] } )
        , ( 11, { hp = 10, buildChances = [ 100, 0, 0 ] } )
        , ( 12, { hp = 10, buildChances = [ 100, 0, 0 ] } )
        , ( 13, { hp = 10, buildChances = [ 100, 0, 0 ] } )
        , ( 14, { hp = 10, buildChances = [ 100, 0, 0 ] } )
        , ( 15, { hp = 10, buildChances = [ 100, 0, 0 ] } )
        , ( 16, { hp = 10, buildChances = [ 100, 0, 0 ] } )
        , ( 17, { hp = 10, buildChances = [ 100, 0, 0 ] } )
        , ( 18, { hp = 10, buildChances = [ 100, 0, 0 ] } )
        , ( 19, { hp = 10, buildChances = [ 100, 0, 0 ] } )
        , ( 20, { hp = 10, buildChances = [ 100, 0, 0 ] } )
        , ( 21, { hp = 10, buildChances = [ 100, 0, 0 ] } )
        , ( 22, { hp = 10, buildChances = [ 100, 0, 0 ] } )
        , ( 23, { hp = 10, buildChances = [ 100, 0, 0 ] } )
        , ( 24, { hp = 10, buildChances = [ 100, 0, 0 ] } )
        , ( 25, { hp = 10, buildChances = [ 100, 0, 0 ] } )
        , ( 26, { hp = 10, buildChances = [ 100, 0, 0 ] } )
        , ( 27, { hp = 10, buildChances = [ 100, 0, 0 ] } )
        , ( 28, { hp = 10, buildChances = [ 100, 0, 0 ] } )
        , ( 29, { hp = 10, buildChances = [ 100, 0, 0 ] } )
        , ( 30, { hp = 10, buildChances = [ 100, 0, 0 ] } )
        ]


getLevelInfo : Int -> LevelInfo
getLevelInfo level =
    levelInfoDict
        |> Dict.get level
        |> Maybe.withDefault { hp = 10, buildChances = [ 100, 0, 0 ] }


viewLevel : Int -> Html msg
viewLevel level =
    let
        levelInfo =
            getLevelInfo level

        chances =
            levelInfo.buildChances |> List.map String.fromInt |> String.join ", "
    in
    tr [] [ td [] [ text (String.fromInt level) ], td [] [ text (String.fromInt levelInfo.hp) ], td [] [ text chances ] ]


viewLevels : Html msg
viewLevels =
    div []
        [ h3 [] [ text "Levels" ]
        , table []
            ([ tr [] [ th [] [ text "Level" ], th [] [ text "Hp" ], th [] [ text "Tower chances" ] ]
             ]
                ++ List.map viewLevel (List.range 1 30)
            )
        ]
