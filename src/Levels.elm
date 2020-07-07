module Levels exposing (LevelInfo, getLevelInfo, numberOfLevels, viewLevels)

import Dict
import Html exposing (Html, div, h3, table, tbody, td, text, th, tr)
import Html.Attributes exposing (class, classList, colspan)


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


numberOfLevels =
    30


baseSpeed =
    125


levelInfoDict =
    Dict.fromList
        [ ( 1, { hp = 6, buildChances = ( 100, 0, 0 ), damage = 2, speed = baseSpeed, enemyCount = 10, evasion = 0, magicImmune = False, flying = False, boss = False } )
        , ( 2, { hp = 16, buildChances = ( 100, 0, 0 ), damage = 2, speed = baseSpeed, enemyCount = 10, evasion = 0, magicImmune = False, flying = False, boss = False } )
        , ( 3, { hp = 32, buildChances = ( 100, 0, 0 ), damage = 2, speed = baseSpeed, enemyCount = 10, evasion = 0, magicImmune = False, flying = False, boss = False } )
        , ( 4, { hp = 43, buildChances = ( 100, 0, 0 ), damage = 2, speed = baseSpeed + 35, enemyCount = 10, evasion = 0, magicImmune = False, flying = False, boss = False } )
        , ( 5, { hp = 55, buildChances = ( 100, 0, 0 ), damage = 2, speed = baseSpeed, enemyCount = 10, evasion = 0, magicImmune = False, flying = True, boss = False } )
        , ( 6, { hp = 120, buildChances = ( 80, 20, 0 ), damage = 2, speed = baseSpeed, enemyCount = 10, evasion = 0, magicImmune = False, flying = False, boss = False } )
        , ( 7, { hp = 205, buildChances = ( 80, 20, 0 ), damage = 2, speed = baseSpeed, enemyCount = 10, evasion = 0, magicImmune = False, flying = False, boss = False } )
        , ( 8, { hp = 340, buildChances = ( 80, 20, 0 ), damage = 2, speed = baseSpeed - 35, enemyCount = 10, evasion = 0, magicImmune = False, flying = False, boss = False } )
        , ( 9, { hp = 435, buildChances = ( 80, 20, 0 ), damage = 2, speed = baseSpeed, enemyCount = 10, evasion = 0, magicImmune = False, flying = False, boss = False } )
        , ( 10, { hp = 2500, buildChances = ( 80, 20, 0 ), damage = 20, speed = baseSpeed - 35, enemyCount = 1, evasion = 0, magicImmune = False, flying = False, boss = True } )
        , ( 11, { hp = 600, buildChances = ( 60, 40, 0 ), damage = 5, speed = baseSpeed, enemyCount = 10, evasion = 0, magicImmune = True, flying = False, boss = False } )
        , ( 12, { hp = 580, buildChances = ( 60, 40, 0 ), damage = 5, speed = baseSpeed + 35, enemyCount = 10, evasion = 20, magicImmune = False, flying = False, boss = False } )
        , ( 13, { hp = 970, buildChances = ( 60, 40, 0 ), damage = 5, speed = baseSpeed, enemyCount = 10, evasion = 0, magicImmune = False, flying = False, boss = False } )
        , ( 14, { hp = 1250, buildChances = ( 60, 40, 0 ), damage = 5, speed = baseSpeed, enemyCount = 10, evasion = 0, magicImmune = False, flying = False, boss = False } )
        , ( 15, { hp = 435, buildChances = ( 60, 40, 0 ), damage = 5, speed = baseSpeed, enemyCount = 10, evasion = 20, magicImmune = True, flying = True, boss = False } )
        , ( 16, { hp = 1650, buildChances = ( 30, 60, 10 ), damage = 5, speed = baseSpeed, enemyCount = 10, evasion = 0, magicImmune = False, flying = False, boss = False } )
        , ( 17, { hp = 2075, buildChances = ( 30, 60, 10 ), damage = 5, speed = baseSpeed, enemyCount = 10, evasion = 0, magicImmune = False, flying = False, boss = False } )
        , ( 18, { hp = 2770, buildChances = ( 30, 60, 10 ), damage = 5, speed = baseSpeed - 35, enemyCount = 10, evasion = 30, magicImmune = False, flying = False, boss = False } )
        , ( 19, { hp = 3025, buildChances = ( 30, 60, 10 ), damage = 5, speed = baseSpeed, enemyCount = 10, evasion = 0, magicImmune = False, flying = False, boss = False } )
        , ( 20, { hp = 3000, buildChances = ( 30, 60, 10 ), damage = 25, speed = baseSpeed, enemyCount = 1, evasion = 0, magicImmune = False, flying = True, boss = True } )
        , ( 21, { hp = 3725, buildChances = ( 30, 60, 10 ), damage = 8, speed = baseSpeed, enemyCount = 10, evasion = 0, magicImmune = True, flying = False, boss = False } )
        , ( 22, { hp = 4500, buildChances = ( 30, 60, 10 ), damage = 8, speed = baseSpeed, enemyCount = 10, evasion = 0, magicImmune = False, flying = False, boss = False } )
        , ( 23, { hp = 5700, buildChances = ( 30, 60, 10 ), damage = 8, speed = baseSpeed, enemyCount = 10, evasion = 50, magicImmune = False, flying = False, boss = False } )
        , ( 24, { hp = 7900, buildChances = ( 25, 60, 15 ), damage = 8, speed = baseSpeed, enemyCount = 10, evasion = 0, magicImmune = False, flying = False, boss = False } )
        , ( 25, { hp = 2100, buildChances = ( 25, 60, 15 ), damage = 8, speed = baseSpeed + 35, enemyCount = 10, evasion = 0, magicImmune = True, flying = True, boss = False } )
        , ( 26, { hp = 9500, buildChances = ( 25, 60, 15 ), damage = 8, speed = baseSpeed, enemyCount = 10, evasion = 50, magicImmune = True, flying = False, boss = False } )
        , ( 27, { hp = 13750, buildChances = ( 25, 60, 15 ), damage = 8, speed = baseSpeed - 35, enemyCount = 10, evasion = 0, magicImmune = False, flying = False, boss = False } )
        , ( 28, { hp = 17500, buildChances = ( 25, 60, 15 ), damage = 8, speed = baseSpeed, enemyCount = 10, evasion = 0, magicImmune = False, flying = False, boss = False } )
        , ( 29, { hp = 3050, buildChances = ( 25, 60, 15 ), damage = 10, speed = baseSpeed - 35, enemyCount = 10, evasion = 50, magicImmune = False, flying = True, boss = False } )
        , ( 30, { hp = 100000, buildChances = ( 25, 60, 15 ), damage = 50, speed = baseSpeed, enemyCount = 1, evasion = 0, magicImmune = True, flying = False, boss = True } )
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


viewLevel : Int -> Int -> Html msg
viewLevel currentLevel level =
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
            , if levelInfo.speed > baseSpeed then
                Just "Faster enemies"

              else
                Nothing
            , if levelInfo.speed < baseSpeed then
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
    tbody [ classList [ ( "current-level", currentLevel == level ) ] ]
        [ tr [ class "level-row" ]
            [ td [] [ text (String.fromInt level) ]
            , td [] [ text (String.fromInt levelInfo.hp) ]
            , td [] [ text (String.fromInt levelInfo.damage) ]
            , td [] [ text (chancesString levelInfo.buildChances) ]
            ]
        , if String.isEmpty description then
            text ""

          else
            tr [] [ td [ class "level-description", colspan 10 ] [ text description ] ]
        ]


viewLevels : Int -> Html msg
viewLevels currentLevel =
    div [ class "card levels-card" ]
        [ h3 [] [ text "Levels" ]
        , table [ class "levels-table" ]
            ([ tr []
                [ th [] [ text "Level" ]
                , th [] [ text "Hp" ]
                , th [] [ text "Damage" ]
                , th [] [ text "Tower chances" ]
                ]
             ]
                ++ List.map (viewLevel currentLevel) (List.range 1 numberOfLevels)
            )
        ]
