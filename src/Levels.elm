module Levels exposing (LevelInfo, getLevelInfo, viewLevels)

import Dict
import Html exposing (Html, div, h3, table, tbody, td, text, th, tr)
import Html.Attributes exposing (class, colspan)


type alias LevelInfo =
    { hp : Int, buildChances : List Int, damage : Int, enemyCount : Int, boss : Bool, flying : Bool, description : String }


levelInfoDict =
    Dict.fromList
        [ ( 1, { hp = 10, buildChances = [ 100, 0, 0 ], damage = 1, enemyCount = 10, flying = False, boss = False, description = "" } )
        , ( 2, { hp = 20, buildChances = [ 100, 0, 0 ], damage = 1, enemyCount = 10, flying = False, boss = False, description = "" } )
        , ( 3, { hp = 35, buildChances = [ 90, 10, 0 ], damage = 1, enemyCount = 10, flying = False, boss = False, description = "" } )
        , ( 4, { hp = 55, buildChances = [ 90, 10, 0 ], damage = 1, enemyCount = 10, flying = False, boss = False, description = "" } )
        , ( 5, { hp = 50, buildChances = [ 90, 10, 0 ], damage = 1, enemyCount = 10, flying = True, boss = False, description = "Flying" } )
        , ( 6, { hp = 100, buildChances = [ 90, 10, 0 ], damage = 1, enemyCount = 10, flying = False, boss = False, description = "" } )
        , ( 7, { hp = 500, buildChances = [ 80, 20, 0 ], damage = 1, enemyCount = 10, flying = False, boss = False, description = "" } )
        , ( 8, { hp = 700, buildChances = [ 80, 20, 0 ], damage = 1, enemyCount = 10, flying = False, boss = False, description = "" } )
        , ( 9, { hp = 1000, buildChances = [ 80, 20, 0 ], damage = 1, enemyCount = 10, flying = False, boss = False, description = "" } )
        , ( 10, { hp = 2000, buildChances = [ 60, 40, 0 ], damage = 1, enemyCount = 1, flying = False, boss = True, description = "Boss" } )
        , ( 11, { hp = 1500, buildChances = [ 60, 40, 0 ], damage = 1, enemyCount = 10, flying = False, boss = False, description = "" } )
        , ( 12, { hp = 2100, buildChances = [ 60, 40, 0 ], damage = 1, enemyCount = 10, flying = False, boss = False, description = "" } )
        , ( 13, { hp = 2800, buildChances = [ 60, 40, 0 ], damage = 1, enemyCount = 10, flying = False, boss = False, description = "" } )
        , ( 14, { hp = 3800, buildChances = [ 50, 45, 5 ], damage = 1, enemyCount = 10, flying = False, boss = False, description = "" } )
        , ( 15, { hp = 3500, buildChances = [ 50, 45, 5 ], damage = 1, enemyCount = 10, flying = True, boss = False, description = "Flying" } )
        , ( 16, { hp = 4500, buildChances = [ 50, 45, 5 ], damage = 1, enemyCount = 10, flying = False, boss = False, description = "" } )
        , ( 17, { hp = 5800, buildChances = [ 40, 50, 10 ], damage = 1, enemyCount = 10, flying = False, boss = False, description = "" } )
        , ( 18, { hp = 7000, buildChances = [ 40, 50, 10 ], damage = 1, enemyCount = 10, flying = False, boss = False, description = "" } )
        , ( 19, { hp = 10, buildChances = [ 40, 50, 10 ], damage = 1, enemyCount = 10, flying = False, boss = False, description = "" } )
        , ( 20, { hp = 10, buildChances = [ 40, 50, 10 ], damage = 1, enemyCount = 1, flying = True, boss = True, description = "Flying Boss" } )
        , ( 21, { hp = 10, buildChances = [ 30, 55, 15 ], damage = 1, enemyCount = 10, flying = False, boss = False, description = "" } )
        , ( 22, { hp = 10, buildChances = [ 30, 55, 15 ], damage = 1, enemyCount = 10, flying = False, boss = False, description = "" } )
        , ( 23, { hp = 10, buildChances = [ 20, 60, 20 ], damage = 1, enemyCount = 10, flying = False, boss = False, description = "" } )
        , ( 24, { hp = 10, buildChances = [ 20, 60, 20 ], damage = 1, enemyCount = 10, flying = False, boss = False, description = "" } )
        , ( 25, { hp = 10, buildChances = [ 20, 60, 20 ], damage = 1, enemyCount = 10, flying = True, boss = False, description = "Flying" } )
        , ( 26, { hp = 10, buildChances = [ 10, 50, 40 ], damage = 1, enemyCount = 10, flying = False, boss = False, description = "" } )
        , ( 27, { hp = 10, buildChances = [ 10, 50, 40 ], damage = 1, enemyCount = 10, flying = False, boss = False, description = "" } )
        , ( 28, { hp = 10, buildChances = [ 10, 50, 40 ], damage = 1, enemyCount = 10, flying = False, boss = False, description = "" } )
        , ( 29, { hp = 10, buildChances = [ 10, 50, 40 ], damage = 1, enemyCount = 10, flying = False, boss = False, description = "" } )
        , ( 30, { hp = 10, buildChances = [ 10, 50, 40 ], damage = 1, enemyCount = 1, flying = False, boss = True, description = "Boss" } )
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
