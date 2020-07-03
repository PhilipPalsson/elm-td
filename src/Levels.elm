module Levels exposing (LevelInfo, getLevelInfo, viewLevels)

import Dict
import Html exposing (Html, div, h3, table, tbody, td, text, th, tr)
import Html.Attributes exposing (class, colspan)


type alias LevelInfo =
    { hp : Int
    , buildChances : ( Int, Int, Int )
    , damage : Int
    , enemyCount : Int
    , boss : Bool
    , flying : Bool
    , evasion : Int
    , speed : Int
    , magicImmune : Bool
    }


levelInfoDict =
    Dict.fromList
        [ ( 1, { hp = 10, buildChances = ( 100, 0, 0 ), damage = 1, speed = 100, enemyCount = 10, evasion = 0, magicImmune = False, flying = False, boss = False } )
        , ( 2, { hp = 20, buildChances = ( 100, 0, 0 ), damage = 1, speed = 100, enemyCount = 10, evasion = 0, magicImmune = False, flying = False, boss = False } )
        , ( 3, { hp = 35, buildChances = ( 100, 0, 0 ), damage = 1, speed = 100, enemyCount = 10, evasion = 0, magicImmune = False, flying = False, boss = False } )
        , ( 4, { hp = 55, buildChances = ( 100, 0, 0 ), damage = 1, speed = 100, enemyCount = 10, evasion = 0, magicImmune = False, flying = False, boss = False } )
        , ( 5, { hp = 50, buildChances = ( 100, 0, 0 ), damage = 1, speed = 100, enemyCount = 10, evasion = 0, magicImmune = False, flying = True, boss = False } )
        , ( 6, { hp = 100, buildChances = ( 100, 0, 0 ), damage = 1, speed = 100, enemyCount = 10, evasion = 0, magicImmune = False, flying = False, boss = False } )
        , ( 7, { hp = 150, buildChances = ( 80, 20, 0 ), damage = 1, speed = 100, enemyCount = 10, evasion = 0, magicImmune = False, flying = False, boss = False } )
        , ( 8, { hp = 210, buildChances = ( 80, 20, 0 ), damage = 1, speed = 100, enemyCount = 10, evasion = 15, magicImmune = False, flying = False, boss = False } )
        , ( 9, { hp = 300, buildChances = ( 80, 20, 0 ), damage = 1, speed = 100, enemyCount = 10, evasion = 0, magicImmune = False, flying = False, boss = False } )
        , ( 10, { hp = 2000, buildChances = ( 80, 20, 0 ), damage = 1, speed = 100, enemyCount = 1, evasion = 0, magicImmune = False, flying = False, boss = True } )
        , ( 11, { hp = 1500, buildChances = ( 80, 20, 0 ), damage = 1, speed = 100, enemyCount = 10, evasion = 0, magicImmune = False, flying = False, boss = False } )
        , ( 12, { hp = 2100, buildChances = ( 80, 20, 0 ), damage = 1, speed = 100, enemyCount = 10, evasion = 0, magicImmune = False, flying = False, boss = False } )
        , ( 13, { hp = 2800, buildChances = ( 60, 40, 0 ), damage = 1, speed = 100, enemyCount = 10, evasion = 0, magicImmune = False, flying = False, boss = False } )
        , ( 14, { hp = 3800, buildChances = ( 60, 40, 0 ), damage = 1, speed = 100, enemyCount = 10, evasion = 0, magicImmune = False, flying = False, boss = False } )
        , ( 15, { hp = 3500, buildChances = ( 60, 40, 0 ), damage = 1, speed = 100, enemyCount = 10, evasion = 15, magicImmune = False, flying = True, boss = False } )
        , ( 16, { hp = 4500, buildChances = ( 60, 40, 0 ), damage = 1, speed = 100, enemyCount = 10, evasion = 0, magicImmune = False, flying = False, boss = False } )
        , ( 17, { hp = 5800, buildChances = ( 60, 40, 0 ), damage = 1, speed = 100, enemyCount = 10, evasion = 0, magicImmune = False, flying = False, boss = False } )
        , ( 18, { hp = 7000, buildChances = ( 60, 40, 0 ), damage = 1, speed = 100, enemyCount = 10, evasion = 0, magicImmune = False, flying = False, boss = False } )
        , ( 19, { hp = 10, buildChances = ( 35, 60, 5 ), damage = 1, speed = 100, enemyCount = 10, evasion = 0, magicImmune = False, flying = False, boss = False } )
        , ( 20, { hp = 10, buildChances = ( 35, 60, 5 ), damage = 1, speed = 100, enemyCount = 1, evasion = 0, magicImmune = False, flying = True, boss = True } )
        , ( 21, { hp = 10, buildChances = ( 35, 60, 5 ), damage = 1, speed = 100, enemyCount = 10, evasion = 0, magicImmune = False, flying = False, boss = False } )
        , ( 22, { hp = 10, buildChances = ( 35, 60, 5 ), damage = 1, speed = 100, enemyCount = 10, evasion = 0, magicImmune = False, flying = False, boss = False } )
        , ( 23, { hp = 10, buildChances = ( 35, 60, 5 ), damage = 1, speed = 100, enemyCount = 10, evasion = 40, magicImmune = False, flying = False, boss = False } )
        , ( 24, { hp = 10, buildChances = ( 35, 60, 5 ), damage = 1, speed = 100, enemyCount = 10, evasion = 0, magicImmune = False, flying = False, boss = False } )
        , ( 25, { hp = 10, buildChances = ( 20, 65, 15 ), damage = 1, speed = 100, enemyCount = 10, evasion = 0, magicImmune = False, flying = True, boss = False } )
        , ( 26, { hp = 10, buildChances = ( 20, 65, 15 ), damage = 1, speed = 100, enemyCount = 10, evasion = 0, magicImmune = False, flying = False, boss = False } )
        , ( 27, { hp = 10, buildChances = ( 20, 65, 15 ), damage = 1, speed = 100, enemyCount = 10, evasion = 50, magicImmune = False, flying = False, boss = False } )
        , ( 28, { hp = 10, buildChances = ( 20, 65, 15 ), damage = 1, speed = 100, enemyCount = 10, evasion = 0, magicImmune = False, flying = False, boss = False } )
        , ( 29, { hp = 10, buildChances = ( 20, 65, 15 ), damage = 1, speed = 100, enemyCount = 10, evasion = 0, magicImmune = False, flying = False, boss = False } )
        , ( 30, { hp = 10, buildChances = ( 20, 65, 15 ), damage = 1, speed = 100, enemyCount = 1, evasion = 0, magicImmune = False, flying = False, boss = True } )
        , ( 31, { hp = 10, buildChances = ( 20, 65, 15 ), damage = 1, speed = 100, enemyCount = 10, evasion = 0, magicImmune = False, flying = True, boss = False } )
        , ( 32, { hp = 10, buildChances = ( 20, 65, 15 ), damage = 1, speed = 100, enemyCount = 10, evasion = 0, magicImmune = False, flying = False, boss = False } )
        , ( 33, { hp = 10, buildChances = ( 20, 65, 15 ), damage = 1, speed = 100, enemyCount = 10, evasion = 50, magicImmune = False, flying = False, boss = False } )
        , ( 34, { hp = 10, buildChances = ( 20, 65, 15 ), damage = 1, speed = 100, enemyCount = 10, evasion = 0, magicImmune = False, flying = False, boss = False } )
        , ( 35, { hp = 10, buildChances = ( 20, 65, 15 ), damage = 1, speed = 100, enemyCount = 1, evasion = 0, magicImmune = False, flying = False, boss = False } )
        ]


getLevelInfo : Int -> LevelInfo
getLevelInfo level =
    levelInfoDict
        |> Dict.get level
        |> Maybe.withDefault
            { hp = 10
            , buildChances = ( 100, 0, 0 )
            , damage = 1
            , enemyCount = 10
            , evasion = 0
            , speed = 100
            , magicImmune = False
            , flying = False
            , boss = False
            }


viewLevel : Int -> Html msg
viewLevel level =
    let
        levelInfo =
            getLevelInfo level

        chancesString ( level1, level2, level3 ) =
            String.fromInt level1
                ++ ", "
                ++ String.fromInt level2
                ++ ", "
                ++ String.fromInt level3

        description =
            [ if levelInfo.boss then
                Just "Boss"

              else
                Nothing
            , if levelInfo.flying then
                Just "Flying"

              else
                Nothing
            , if levelInfo.magicImmune then
                Just "Slow immune"

              else
                Nothing
            , if levelInfo.speed > 100 then
                Just "Faster enemies"

              else
                Nothing
            , if levelInfo.speed < 100 then
                Just "Slower enemies"

              else
                Nothing
            , if levelInfo.evasion > 0 then
                Just ("Evasion " ++ String.fromInt levelInfo.evasion ++ "%")

              else
                Nothing
            ]
                |> List.filterMap identity
                |> String.join ", "
    in
    tbody []
        [ tr [ class "level-row" ]
            [ td [] [ text (String.fromInt level) ]
            , td [] [ text (String.fromInt levelInfo.hp) ]
            , td [] [ text (String.fromInt levelInfo.damage) ]
            , td [] [ text (chancesString levelInfo.buildChances) ]
            ]
        , tr [] [ td [ class "level-description", colspan 10 ] [ text description ] ]
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
