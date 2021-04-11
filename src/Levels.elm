module Levels exposing (LevelInfo, getLevelInfo, numberOfLevels, viewLevels)

import Dict exposing (Dict)
import Html exposing (Html, div, h3, table, tbody, td, text, th, tr)
import Html.Attributes exposing (class, classList, colspan)


type alias LevelInfo =
    { hp : Int
    , buildChances : ( Int, Int, Int )
    , damage : Int
    , boss : Bool
    , flying : Bool
    , evasion : Int
    , speed : Int
    , magicImmune : Bool
    }


numberOfLevels : Int
numberOfLevels =
    Dict.size levelInfoDict


baseSpeed : number
baseSpeed =
    130


levelInfoDict : Dict Int LevelInfo
levelInfoDict =
    Dict.fromList
        ([ { hp = 6, buildChances = ( 100, 0, 0 ), damage = 2, speed = baseSpeed, evasion = 0, magicImmune = False, flying = False, boss = False }
         , { hp = 16, buildChances = ( 100, 0, 0 ), damage = 2, speed = baseSpeed, evasion = 0, magicImmune = False, flying = False, boss = False }
         , { hp = 32, buildChances = ( 100, 0, 0 ), damage = 2, speed = baseSpeed, evasion = 0, magicImmune = False, flying = False, boss = False }
         , { hp = 43, buildChances = ( 100, 0, 0 ), damage = 2, speed = baseSpeed + 35, evasion = 0, magicImmune = False, flying = False, boss = False }
         , { hp = 55, buildChances = ( 100, 0, 0 ), damage = 2, speed = baseSpeed, evasion = 0, magicImmune = False, flying = True, boss = False }
         , { hp = 120, buildChances = ( 80, 20, 0 ), damage = 2, speed = baseSpeed, evasion = 0, magicImmune = False, flying = False, boss = False }
         , { hp = 205, buildChances = ( 80, 20, 0 ), damage = 2, speed = baseSpeed, evasion = 0, magicImmune = False, flying = False, boss = False }
         , { hp = 340, buildChances = ( 80, 20, 0 ), damage = 2, speed = baseSpeed - 35, evasion = 0, magicImmune = False, flying = False, boss = False }
         , { hp = 420, buildChances = ( 80, 20, 0 ), damage = 2, speed = baseSpeed, evasion = 0, magicImmune = False, flying = False, boss = False }
         , { hp = 2900, buildChances = ( 80, 20, 0 ), damage = 20, speed = baseSpeed - 35, evasion = 0, magicImmune = False, flying = False, boss = True }
         , { hp = 600, buildChances = ( 60, 40, 0 ), damage = 5, speed = baseSpeed, evasion = 0, magicImmune = True, flying = False, boss = False }
         , { hp = 580, buildChances = ( 60, 40, 0 ), damage = 5, speed = baseSpeed + 35, evasion = 20, magicImmune = False, flying = False, boss = False }
         , { hp = 970, buildChances = ( 60, 40, 0 ), damage = 5, speed = baseSpeed, evasion = 0, magicImmune = False, flying = False, boss = False }
         , { hp = 1250, buildChances = ( 60, 40, 0 ), damage = 5, speed = baseSpeed, evasion = 0, magicImmune = False, flying = False, boss = False }
         , { hp = 435, buildChances = ( 60, 40, 0 ), damage = 5, speed = baseSpeed, evasion = 20, magicImmune = True, flying = True, boss = False }
         , { hp = 1650, buildChances = ( 30, 60, 10 ), damage = 5, speed = baseSpeed, evasion = 0, magicImmune = False, flying = False, boss = False }
         , { hp = 2075, buildChances = ( 30, 60, 10 ), damage = 5, speed = baseSpeed, evasion = 0, magicImmune = False, flying = False, boss = False }
         , { hp = 2770, buildChances = ( 30, 60, 10 ), damage = 5, speed = baseSpeed - 35, evasion = 30, magicImmune = False, flying = False, boss = False }
         , { hp = 3025, buildChances = ( 30, 60, 10 ), damage = 5, speed = baseSpeed, evasion = 0, magicImmune = False, flying = False, boss = False }
         , { hp = 3000, buildChances = ( 30, 60, 10 ), damage = 25, speed = baseSpeed, evasion = 0, magicImmune = False, flying = True, boss = True }
         , { hp = 3725, buildChances = ( 30, 60, 10 ), damage = 8, speed = baseSpeed, evasion = 0, magicImmune = True, flying = False, boss = False }
         , { hp = 4500, buildChances = ( 30, 60, 10 ), damage = 8, speed = baseSpeed, evasion = 0, magicImmune = False, flying = False, boss = False }
         , { hp = 5500, buildChances = ( 30, 60, 10 ), damage = 8, speed = baseSpeed, evasion = 50, magicImmune = False, flying = False, boss = False }
         , { hp = 7900, buildChances = ( 25, 60, 15 ), damage = 8, speed = baseSpeed, evasion = 0, magicImmune = False, flying = False, boss = False }
         , { hp = 2100, buildChances = ( 25, 60, 15 ), damage = 8, speed = baseSpeed + 35, evasion = 0, magicImmune = True, flying = True, boss = False }
         , { hp = 8700, buildChances = ( 25, 60, 15 ), damage = 8, speed = baseSpeed, evasion = 50, magicImmune = True, flying = False, boss = False }
         , { hp = 13750, buildChances = ( 25, 60, 15 ), damage = 8, speed = baseSpeed - 35, evasion = 0, magicImmune = False, flying = False, boss = False }
         , { hp = 17500, buildChances = ( 25, 60, 15 ), damage = 8, speed = baseSpeed, evasion = 0, magicImmune = False, flying = False, boss = False }
         , { hp = 3050, buildChances = ( 25, 60, 15 ), damage = 10, speed = baseSpeed - 35, evasion = 50, magicImmune = False, flying = True, boss = False }
         , { hp = 60000, buildChances = ( 25, 60, 15 ), damage = 40, speed = baseSpeed, evasion = 0, magicImmune = True, flying = False, boss = True }
         , { hp = 120000, buildChances = ( 20, 50, 30 ), damage = 40, speed = baseSpeed - 35, evasion = 0, magicImmune = False, flying = False, boss = True }
         , { hp = 7500, buildChances = ( 20, 50, 30 ), damage = 40, speed = baseSpeed + 35, evasion = 0, magicImmune = False, flying = True, boss = True }
         ]
            |> List.indexedMap (\i l -> ( i + 1, l ))
        )


getLevelInfo : Int -> LevelInfo
getLevelInfo level =
    levelInfoDict
        |> Dict.get level
        |> Maybe.withDefault
            { hp = 10
            , buildChances = ( 100, 0, 0 )
            , damage = 1
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
