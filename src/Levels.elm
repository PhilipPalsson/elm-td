module Levels exposing (LevelInfo, getLevelInfo, viewLevels)

import Dict
import Html exposing (Html, div, h3, table, td, text, th, tr)
import Html.Attributes exposing (class)


type alias LevelInfo =
    { hp : Int, buildChances : List Int, damage : Int }


levelInfoDict =
    Dict.fromList
        [ ( 1, { hp = 100000, buildChances = [ 100, 0, 0 ], damage = 50 } )
        , ( 2, { hp = 50, buildChances = [ 100, 0, 0 ], damage = 1 } )
        , ( 3, { hp = 100, buildChances = [ 100, 0, 0 ], damage = 1 } )
        , ( 4, { hp = 500, buildChances = [ 100, 0, 0 ], damage = 1 } )
        , ( 5, { hp = 1000, buildChances = [ 100, 0, 0 ], damage = 1 } )
        , ( 6, { hp = 5000, buildChances = [ 100, 0, 0 ], damage = 1 } )
        , ( 7, { hp = 10, buildChances = [ 100, 0, 0 ], damage = 1 } )
        , ( 8, { hp = 10, buildChances = [ 100, 0, 0 ], damage = 1 } )
        , ( 9, { hp = 10, buildChances = [ 100, 0, 0 ], damage = 1 } )
        , ( 10, { hp = 10, buildChances = [ 100, 0, 0 ], damage = 1 } )
        , ( 11, { hp = 10, buildChances = [ 100, 0, 0 ], damage = 1 } )
        , ( 12, { hp = 10, buildChances = [ 100, 0, 0 ], damage = 1 } )
        , ( 13, { hp = 10, buildChances = [ 100, 0, 0 ], damage = 1 } )
        , ( 14, { hp = 10, buildChances = [ 100, 0, 0 ], damage = 1 } )
        , ( 15, { hp = 10, buildChances = [ 100, 0, 0 ], damage = 1 } )
        , ( 16, { hp = 10, buildChances = [ 100, 0, 0 ], damage = 1 } )
        , ( 17, { hp = 10, buildChances = [ 100, 0, 0 ], damage = 1 } )
        , ( 18, { hp = 10, buildChances = [ 100, 0, 0 ], damage = 1 } )
        , ( 19, { hp = 10, buildChances = [ 100, 0, 0 ], damage = 1 } )
        , ( 20, { hp = 10, buildChances = [ 100, 0, 0 ], damage = 1 } )
        , ( 21, { hp = 10, buildChances = [ 100, 0, 0 ], damage = 1 } )
        , ( 22, { hp = 10, buildChances = [ 100, 0, 0 ], damage = 1 } )
        , ( 23, { hp = 10, buildChances = [ 100, 0, 0 ], damage = 1 } )
        , ( 24, { hp = 10, buildChances = [ 100, 0, 0 ], damage = 1 } )
        , ( 25, { hp = 10, buildChances = [ 100, 0, 0 ], damage = 1 } )
        , ( 26, { hp = 10, buildChances = [ 100, 0, 0 ], damage = 1 } )
        , ( 27, { hp = 10, buildChances = [ 100, 0, 0 ], damage = 1 } )
        , ( 28, { hp = 10, buildChances = [ 100, 0, 0 ], damage = 1 } )
        , ( 29, { hp = 10, buildChances = [ 100, 0, 0 ], damage = 1 } )
        , ( 30, { hp = 10, buildChances = [ 100, 0, 0 ], damage = 1 } )
        ]


getLevelInfo : Int -> LevelInfo
getLevelInfo level =
    levelInfoDict
        |> Dict.get level
        |> Maybe.withDefault { hp = 10, buildChances = [ 100, 0, 0 ], damage = 1 }


viewLevel : Int -> Html msg
viewLevel level =
    let
        levelInfo =
            getLevelInfo level

        chances =
            levelInfo.buildChances |> List.map String.fromInt |> String.join ", "
    in
    tr []
        [ td [] [ text (String.fromInt level) ]
        , td [] [ text (String.fromInt levelInfo.hp) ]
        , td [] [ text (String.fromInt levelInfo.damage) ]
        , td [] [ text chances ]
        ]


viewLevels : Html msg
viewLevels =
    div []
        [ h3 [] [ text "Levels" ]
        , table [ class "levels-table" ]
            ([ tr []
                [ th [] [ text "Level" ]
                , th [] [ text "Hp" ]
                , th [] [ text "Damage" ]
                , th [] [ text "Tower chances" ]
                ]
             ]
                ++ List.map viewLevel (List.range 1 30)
            )
        ]
