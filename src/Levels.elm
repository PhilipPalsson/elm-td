module Levels exposing (LevelInfo, getLevelInfo, viewLevels)

import Dict
import Html exposing (Html, div, h3, table, tbody, td, text, th, tr)
import Html.Attributes exposing (class, colspan)


type alias LevelInfo =
    { hp : Int, buildChances : List Int, damage : Int, enemyCount : Int, boss : Bool, flying : Bool, description : String }


levelInfoDict =
    Dict.fromList
        [ ( 1, { hp = 30, buildChances = [ 100, 0, 0 ], damage = 1, enemyCount = 1, flying = True, boss = True, description = "" } )
        , ( 2, { hp = 50, buildChances = [ 100, 0, 0 ], damage = 1, enemyCount = 10, flying = False, boss = False, description = "" } )
        , ( 3, { hp = 100, buildChances = [ 100, 0, 0 ], damage = 1, enemyCount = 10, flying = False, boss = False, description = "" } )
        , ( 4, { hp = 500, buildChances = [ 100, 0, 0 ], damage = 1, enemyCount = 10, flying = False, boss = False, description = "" } )
        , ( 5, { hp = 1000, buildChances = [ 100, 0, 0 ], damage = 1, enemyCount = 10, flying = True, boss = False, description = "Flying" } )
        , ( 6, { hp = 5000, buildChances = [ 100, 0, 0 ], damage = 1, enemyCount = 10, flying = False, boss = False, description = "" } )
        , ( 7, { hp = 10, buildChances = [ 100, 0, 0 ], damage = 1, enemyCount = 10, flying = False, boss = False, description = "" } )
        , ( 8, { hp = 10, buildChances = [ 100, 0, 0 ], damage = 1, enemyCount = 10, flying = False, boss = False, description = "" } )
        , ( 9, { hp = 10, buildChances = [ 100, 0, 0 ], damage = 1, enemyCount = 10, flying = False, boss = False, description = "" } )
        , ( 10, { hp = 10, buildChances = [ 100, 0, 0 ], damage = 1, enemyCount = 10, flying = False, boss = True, description = "Boss" } )
        , ( 11, { hp = 10, buildChances = [ 100, 0, 0 ], damage = 1, enemyCount = 10, flying = False, boss = False, description = "" } )
        , ( 12, { hp = 10, buildChances = [ 100, 0, 0 ], damage = 1, enemyCount = 10, flying = False, boss = False, description = "" } )
        , ( 13, { hp = 10, buildChances = [ 100, 0, 0 ], damage = 1, enemyCount = 10, flying = False, boss = False, description = "" } )
        , ( 14, { hp = 10, buildChances = [ 100, 0, 0 ], damage = 1, enemyCount = 10, flying = False, boss = False, description = "" } )
        , ( 15, { hp = 10, buildChances = [ 100, 0, 0 ], damage = 1, enemyCount = 10, flying = True, boss = False, description = "Flying" } )
        , ( 16, { hp = 10, buildChances = [ 100, 0, 0 ], damage = 1, enemyCount = 10, flying = False, boss = False, description = "" } )
        , ( 17, { hp = 10, buildChances = [ 100, 0, 0 ], damage = 1, enemyCount = 10, flying = False, boss = False, description = "" } )
        , ( 18, { hp = 10, buildChances = [ 100, 0, 0 ], damage = 1, enemyCount = 10, flying = False, boss = False, description = "" } )
        , ( 19, { hp = 10, buildChances = [ 100, 0, 0 ], damage = 1, enemyCount = 10, flying = False, boss = False, description = "" } )
        , ( 20, { hp = 10, buildChances = [ 100, 0, 0 ], damage = 1, enemyCount = 10, flying = True, boss = True, description = "Flying Boss" } )
        , ( 21, { hp = 10, buildChances = [ 100, 0, 0 ], damage = 1, enemyCount = 10, flying = False, boss = False, description = "" } )
        , ( 22, { hp = 10, buildChances = [ 100, 0, 0 ], damage = 1, enemyCount = 10, flying = False, boss = False, description = "" } )
        , ( 23, { hp = 10, buildChances = [ 100, 0, 0 ], damage = 1, enemyCount = 10, flying = False, boss = False, description = "" } )
        , ( 24, { hp = 10, buildChances = [ 100, 0, 0 ], damage = 1, enemyCount = 10, flying = False, boss = False, description = "" } )
        , ( 25, { hp = 10, buildChances = [ 100, 0, 0 ], damage = 1, enemyCount = 10, flying = True, boss = False, description = "Flying" } )
        , ( 26, { hp = 10, buildChances = [ 100, 0, 0 ], damage = 1, enemyCount = 10, flying = False, boss = False, description = "" } )
        , ( 27, { hp = 10, buildChances = [ 100, 0, 0 ], damage = 1, enemyCount = 10, flying = False, boss = False, description = "" } )
        , ( 28, { hp = 10, buildChances = [ 100, 0, 0 ], damage = 1, enemyCount = 10, flying = False, boss = False, description = "" } )
        , ( 29, { hp = 10, buildChances = [ 100, 0, 0 ], damage = 1, enemyCount = 10, flying = False, boss = False, description = "" } )
        , ( 30, { hp = 10, buildChances = [ 100, 0, 0 ], damage = 1, enemyCount = 10, flying = False, boss = True, description = "Boss" } )
        ]


getLevelInfo : Int -> LevelInfo
getLevelInfo level =
    levelInfoDict
        |> Dict.get level
        |> Maybe.withDefault { hp = 10, buildChances = [ 100, 0, 0 ], damage = 1, enemyCount = 10, flying = False, boss = False, description = "" }


viewLevel : Int -> Html msg
viewLevel level =
    let
        levelInfo =
            getLevelInfo level

        chances =
            levelInfo.buildChances |> List.map String.fromInt |> String.join ", "
    in
    tbody []
        [ tr [ class "level-row" ]
            [ td [] [ text (String.fromInt level) ]
            , td [] [ text (String.fromInt levelInfo.hp) ]
            , td [] [ text (String.fromInt levelInfo.damage) ]
            , td [] [ text chances ]
            ]
        , tr
            []
            [ td [ class "level-description", colspan 10 ]
                [ text
                    (if String.isEmpty levelInfo.description then
                        ""

                     else
                        "Special: " ++ levelInfo.description
                    )
                ]
            ]
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
