module Tower exposing (createTower, getTowerData, getTowerType, viewTower, viewTowerInformation)

import Array
import Constants exposing (cellSize)
import Dict
import Helper exposing (actionButtonsPosition, intToPxString)
import Html exposing (Html, button, div, h3, table, td, text, th, tr)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick, stopPropagationOn)
import Json.Decode
import Random exposing (Seed)
import TowerTypes exposing (Tower, TowerEffect(..), TowerId, TowerType(..))
import Types exposing (CellIndex, GameMsg(..), GameState(..), Towers)


basicTowers : List TowerType
basicTowers =
    [ Red1
    , Red2
    , Red3
    , Green1
    , Green2
    , Green3
    , Blue1
    , Blue2
    , Blue3
    , Black1
    , Black2
    , Black3
    ]


combinedTowers : List TowerType
combinedTowers =
    [ White
    , Teal
    , Orange
    , Purple
    , Grey
    , DarkBlue
    , Pink
    , Yellow
    , Gold
    ]


type alias TowerData =
    { name : String
    , range : Int
    , damage : Int
    , rate : Int
    , effects : List TowerEffect
    , targets : Int
    , color : String
    , combinations : List TowerType
    }


getTowerData : TowerType -> TowerData
getTowerData towerType =
    case towerType of
        Red1 ->
            { name = "Red 1"
            , range = 100
            , damage = 6
            , rate = 100
            , effects = [ SpeedAura 10 ]
            , targets = 1
            , color = "red"
            , combinations = []
            }

        Red2 ->
            { name = "Red 2"
            , range = 100
            , damage = 12
            , rate = 100
            , effects = [ SpeedAura 20 ]
            , targets = 1
            , color = "red"
            , combinations = []
            }

        Red3 ->
            { name = "Red 3"
            , range = 100
            , damage = 36
            , rate = 100
            , effects = [ SpeedAura 30 ]
            , targets = 1
            , color = "red"
            , combinations = []
            }

        Green1 ->
            { name = "Green 1"
            , range = 110
            , damage = 15
            , rate = 100
            , effects = []
            , targets = 1
            , color = "#19e219"
            , combinations = []
            }

        Green2 ->
            { name = "Green 2"
            , range = 110
            , damage = 30
            , rate = 100
            , effects = []
            , targets = 1
            , color = "#19e219"
            , combinations = []
            }

        Green3 ->
            { name = "Green 3"
            , range = 110
            , damage = 90
            , rate = 100
            , effects = []
            , targets = 1
            , color = "#19e219"
            , combinations = []
            }

        Blue1 ->
            { name = "Blue 1"
            , range = 130
            , damage = 8
            , rate = 100
            , effects = [ SlowEffect 10 ]
            , targets = 1
            , color = "#008eff"
            , combinations = []
            }

        Blue2 ->
            { name = "Blue 2"
            , range = 130
            , damage = 16
            , rate = 100
            , effects = [ SlowEffect 20 ]
            , targets = 1
            , color = "#008eff"
            , combinations = []
            }

        Blue3 ->
            { name = "Blue 3"
            , range = 130
            , damage = 48
            , rate = 100
            , effects = [ SlowEffect 30 ]
            , targets = 1
            , color = "#008eff"
            , combinations = []
            }

        Black1 ->
            { name = "Black 1"
            , range = 110
            , damage = 5
            , rate = 100
            , effects = []
            , targets = 2
            , color = "black"
            , combinations = []
            }

        Black2 ->
            { name = "Black 2"
            , range = 110
            , damage = 10
            , rate = 100
            , effects = []
            , targets = 3
            , color = "black"
            , combinations = []
            }

        Black3 ->
            { name = "Black 3"
            , range = 110
            , damage = 30
            , rate = 100
            , effects = []
            , targets = 4
            , color = "black"
            , combinations = []
            }

        White ->
            { name = "White"
            , range = 125
            , damage = 32
            , rate = 130
            , effects = []
            , targets = 1
            , color = "white"
            , combinations = [ Blue1, Red1, Green1 ]
            }

        Teal ->
            { name = "Teal"
            , range = 125
            , damage = 23
            , rate = 140
            , effects = []
            , targets = 3
            , color = "teal"
            , combinations = [ Blue1, Black1, Green1 ]
            }

        Orange ->
            { name = "Orange"
            , range = 100
            , damage = 60
            , rate = 120
            , effects = [ SpeedAura 20 ]
            , targets = 1
            , color = "orange"
            , combinations = [ Red1, Red2, Green2 ]
            }

        Purple ->
            { name = "Purple"
            , range = 125
            , damage = 80
            , rate = 110
            , effects = [ TrueStrike ]
            , targets = 1
            , color = "purple"
            , combinations = [ Blue2, Black2, Red2 ]
            }

        Grey ->
            { name = "Grey"
            , range = 125
            , damage = 55
            , rate = 150
            , effects = [ FlyingDamage 1.8 ]
            , targets = 5
            , color = "darkgrey"
            , combinations = [ Black1, Black2, White ]
            }

        DarkBlue ->
            { name = "Dark Blue"
            , range = 85
            , damage = 40
            , rate = 60
            , effects = [ SlowEffect 50 ]
            , targets = 10
            , color = "darkblue"
            , combinations = [ Blue2, Blue3, Black3 ]
            }

        Pink ->
            { name = "Pink"
            , range = 180
            , damage = 135
            , rate = 145
            , effects = [ TrueStrike, SpeedAura 40 ]
            , targets = 1
            , color = "pink"
            , combinations = [ Red3, White, Purple ]
            }

        Yellow ->
            { name = "Yellow"
            , range = 150
            , damage = 170
            , rate = 160
            , effects = []
            , targets = 1
            , color = "yellow"
            , combinations = [ Green3, White, Orange ]
            }

        Gold ->
            { name = "Gold"
            , range = 160
            , damage = 180
            , rate = 160
            , effects = []
            , targets = 4
            , color = "gold"
            , combinations = [ Black1, Black2, Yellow ]
            }


getTowerType : Seed -> ( Int, Int, Int ) -> ( Seed, TowerType )
getTowerType seed ( chanceLevel1, chanceLevel2, chanceLevel3 ) =
    let
        towerDistributionList =
            List.repeat (chanceLevel1 // 4) Red1
                ++ List.repeat (chanceLevel1 // 4) Green1
                ++ List.repeat (chanceLevel1 // 4) Blue1
                ++ List.repeat (chanceLevel1 // 4) Black1
                ++ List.repeat (chanceLevel2 // 4) Red2
                ++ List.repeat (chanceLevel2 // 4) Green2
                ++ List.repeat (chanceLevel2 // 4) Blue2
                ++ List.repeat (chanceLevel2 // 4) Black2
                ++ List.repeat (chanceLevel3 // 4) Red3
                ++ List.repeat (chanceLevel3 // 4) Green3
                ++ List.repeat (chanceLevel3 // 4) Blue3
                ++ List.repeat (chanceLevel3 // 4) Black3

        ( random, newSeed ) =
            Random.step (Random.int 0 (List.length towerDistributionList)) seed
    in
    ( newSeed
    , towerDistributionList
        |> Array.fromList
        |> Array.get random
        |> Maybe.withDefault Red1
    )


towerCombinations : List ( TowerType, List TowerType )
towerCombinations =
    List.map
        (\towerType ->
            ( towerType
            , (getTowerData towerType).combinations
            )
        )
        combinedTowers


effectString : TowerEffect -> String
effectString towerEffect =
    case towerEffect of
        SlowEffect effect ->
            "Slow effect " ++ String.fromInt effect ++ "%"

        SpeedAura effect ->
            "Speed aura " ++ String.fromInt effect ++ "%"

        FlyingDamage extra ->
            "Flying damage " ++ String.fromInt extra ++ "%"

        TrueStrike ->
            "Ignore evasion"


viewTowerInformation : List TowerType -> List TowerType -> Html GameMsg
viewTowerInformation temporaryTowerTypes existingTowerTypes =
    let
        haveTemporarily towerType =
            List.member towerType temporaryTowerTypes

        have towerType =
            List.member towerType existingTowerTypes

        towerBlock towerType =
            let
                tower =
                    createTower towerType False 0

                hitsPerSecond =
                    toFloat tower.rate / 100

                dps =
                    hitsPerSecond * toFloat tower.damage

                totalDps =
                    dps * toFloat tower.targets

                combinations =
                    (getTowerData towerType).combinations

                specialText =
                    tower.effects |> List.map effectString |> String.join ", "
            in
            div [ class "card" ]
                [ div [ class "tower-block" ]
                    [ div [ class "tower-block-image" ]
                        [ viewTowerOutsideOfBoard tower
                        ]
                    , table [ class "tower-info" ]
                        [ tr []
                            [ th [] [ text "Damage" ]
                            , th [] [ text "Targets" ]
                            , th [] [ text "Range" ]
                            , th [] [ text "Rate" ]
                            , th [] [ text "Dps" ]
                            ]
                        , tr []
                            [ td [] [ text (String.fromInt tower.damage) ]
                            , td [] [ text (String.fromInt tower.targets) ]
                            , td [] [ text (String.fromInt tower.range) ]
                            , td [] [ text (String.fromInt tower.rate) ]
                            , td []
                                [ text
                                    (String.fromInt (round dps)
                                        ++ (if dps /= totalDps then
                                                "/" ++ String.fromInt (round totalDps)

                                            else
                                                ""
                                           )
                                    )
                                ]
                            ]
                        ]
                    ]
                , if String.isEmpty specialText then
                    text ""

                  else
                    div
                        [ class "special-text" ]
                        [ text specialText
                        ]
                , if List.isEmpty combinations then
                    text ""

                  else
                    div [ class "tower-images" ]
                        (List.map
                            (\tt ->
                                div
                                    [ class "tower-block-image"
                                    , class
                                        (towerInfoClass (haveTemporarily tt) (have tt))
                                    ]
                                    [ viewTowerOutsideOfBoard (createTower tt False 0) ]
                            )
                            combinations
                        )
                ]
    in
    div []
        [ h3 []
            [ text "Base towers"
            ]
        , div [ class "tower-list" ] (List.map towerBlock basicTowers)
        , h3 []
            [ text "Combined towers"
            ]
        , div [ class "tower-list" ] (List.map towerBlock combinedTowers)
        ]


towerInfoClass : Bool -> Bool -> String
towerInfoClass haveTemporarily have =
    if have && haveTemporarily then
        "have have-temporarily"

    else if have then
        "have"

    else if haveTemporarily then
        "have-temporarily"

    else
        ""


createTower : TowerType -> Bool -> Int -> Tower
createTower towerType temporary cellIndex =
    let
        towerData =
            getTowerData towerType
    in
    { name = towerData.name
    , damage = towerData.damage
    , totalDamage = 0
    , range = towerData.range
    , cellIndex = cellIndex
    , rate = towerData.rate
    , cooldown = 0
    , targets = towerData.targets
    , temporary = temporary
    , towerType = towerType
    , effects = towerData.effects
    , color = towerData.color
    }


viewTowerOutsideOfBoard : Tower -> Html GameMsg
viewTowerOutsideOfBoard tower =
    viewTower Paused False Dict.empty 0 tower


viewTower : GameState -> Bool -> Towers -> TowerId -> Tower -> Html GameMsg
viewTower state selected towers towerId tower =
    let
        ( barCount, shouldHaveBlock ) =
            case tower.towerType of
                Red1 ->
                    ( 1, False )

                Red2 ->
                    ( 2, False )

                Red3 ->
                    ( 3, False )

                Green1 ->
                    ( 1, False )

                Green2 ->
                    ( 2, False )

                Green3 ->
                    ( 3, False )

                Blue1 ->
                    ( 1, False )

                Blue2 ->
                    ( 2, False )

                Blue3 ->
                    ( 3, False )

                Black1 ->
                    ( 1, False )

                Black2 ->
                    ( 2, False )

                Black3 ->
                    ( 3, False )

                White ->
                    ( 0, True )

                Teal ->
                    ( 0, True )

                Purple ->
                    ( 0, True )

                Orange ->
                    ( 0, True )

                Grey ->
                    ( 0, True )

                DarkBlue ->
                    ( 0, True )

                Pink ->
                    ( 0, True )

                Yellow ->
                    ( 0, True )

                Gold ->
                    ( 0, True )

        bar index =
            div
                [ class "bar"
                , style "bottom" (intToPxString ((index * 6) - 2))
                , style "background-color" tower.color
                ]
                []

        bars =
            List.map bar (List.range 1 barCount)

        block =
            if shouldHaveBlock then
                div
                    [ class "block"
                    , style "background-color" tower.color
                    ]
                    []

            else
                text ""

        upgrades =
            availableUpgrades (List.map .towerType (Dict.values towers)) tower.towerType
    in
    div
        [ class "tower"
        , class
            (if tower.temporary then
                "temporary"

             else
                ""
            )
        ]
        ([ div
            [ class "tower-image"
            , class
                (if tower.temporary || List.isEmpty upgrades || state /= Level then
                    ""

                 else
                    "upgradable"
                )
            ]
            []
         ]
            ++ bars
            ++ [ block ]
            ++ (if selected then
                    [ div
                        [ class "tower-range"
                        , style "width" (intToPxString (tower.range * 2))
                        , style "height" (intToPxString (tower.range * 2))
                        ]
                        []
                    , div
                        [ class "selection"
                        , style "width" (intToPxString (cellSize + 5))
                        , style "height" (intToPxString (cellSize + 5))
                        ]
                        []
                    ]
                        ++ [ div [ class "action-buttons", actionButtonsPosition tower.cellIndex ]
                                (case state of
                                    Build towersLeft ->
                                        if tower.temporary then
                                            if towersLeft == 0 then
                                                [ button
                                                    [ stopPropagationOn "click"
                                                        (Json.Decode.succeed ( KeepTowerClicked towerId, True ))
                                                    ]
                                                    [ text "Keep" ]
                                                ]

                                            else
                                                []

                                        else
                                            [ button
                                                [ onClick (RemoveTowerButtonClicked towerId tower.cellIndex) ]
                                                [ text "Remove" ]
                                            ]

                                    Level ->
                                        List.map
                                            (\upgrade ->
                                                button
                                                    [ stopPropagationOn "click"
                                                        (Json.Decode.succeed ( UpgradeTowerClicked towerId upgrade, True ))
                                                    ]
                                                    [ text (getTowerData upgrade).name ]
                                            )
                                            upgrades

                                    _ ->
                                        []
                                )
                           ]

                else
                    []
               )
        )


availableUpgrades : List TowerType -> TowerType -> List TowerType
availableUpgrades existingTowerType forTower =
    let
        buildable : ( TowerType, List TowerType ) -> Bool
        buildable ( _, components ) =
            List.all (\c -> List.member c existingTowerType) components
                && List.member forTower components
    in
    towerCombinations
        |> List.filter buildable
        |> List.map Tuple.first
