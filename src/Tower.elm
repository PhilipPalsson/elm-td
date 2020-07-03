module Tower exposing (..)

import Constants exposing (cellSize, fps)
import Helper exposing (intToPxString)
import Html exposing (Html, div, h3, table, td, text, th, tr)
import Html.Attributes exposing (class, style)
import List.Extra
import Random exposing (Seed)


type alias Tower =
    { damage : Int
    , totalDamage : Int
    , range : Int
    , cellIndex : Int
    , rate : Int
    , cooldown : Int
    , targets : Int
    , temporary : Bool
    , towerType : TowerType
    , effects : List TowerEffect
    }


type alias TowerId =
    Int


type Color
    = Green
    | Red
    | Blue
    | Black


colors =
    [ Red, Green, Blue, Black ]


maxTowerLevel =
    3


type CombinedTower
    = White
    | Teal
    | Purple
    | Orange
    | Grey
    | DarkBlue
    | Pink
    | Yellow
    | Gold


type TowerType
    = Base Color Int
    | Combined CombinedTower


type TowerEffect
    = FlyingDamage Float
    | SpeedAura Int
    | SlowEffect Int
    | TrueStrike


basicTowers : List TowerType
basicTowers =
    List.foldl (++) [] (List.map (\color -> List.map (Base color) (List.range 1 maxTowerLevel)) colors)


combinedTowers : List TowerType
combinedTowers =
    [ Combined White
    , Combined Teal
    , Combined Orange
    , Combined Purple
    , Combined Grey
    , Combined DarkBlue
    , Combined Pink
    , Combined Yellow
    , Combined Gold
    ]


colorToCssString : Color -> String
colorToCssString color =
    case color of
        Green ->
            "green"

        Red ->
            "red"

        Blue ->
            "blue"

        Black ->
            "black"


colorToString : Color -> String
colorToString color =
    case color of
        Green ->
            "Green"

        Red ->
            "Red"

        Blue ->
            "Blue"

        Black ->
            "Black"


towerTypeToCssString : TowerType -> String
towerTypeToCssString towerType =
    case towerType of
        Base color _ ->
            colorToCssString color

        Combined Purple ->
            "purple"

        Combined White ->
            "white"

        Combined Pink ->
            "pink"

        Combined Yellow ->
            "yellow"

        Combined Orange ->
            "orange"

        Combined Teal ->
            "teal"

        Combined DarkBlue ->
            "darkblue"

        Combined Grey ->
            "darkgrey"

        Combined Gold ->
            "gold"


towerTypeString : TowerType -> String
towerTypeString towerType =
    case towerType of
        Base color level ->
            colorToString color ++ " " ++ String.fromInt level

        Combined combinedTower ->
            combinedTowerTypeString combinedTower


towerTypeFromString : String -> TowerType
towerTypeFromString string =
    case string of
        "Red 1" ->
            Base Red 1

        "Red 2" ->
            Base Red 2

        "Red 3" ->
            Base Red 3

        "Green 1" ->
            Base Green 1

        "Green 2" ->
            Base Green 2

        "Green 3" ->
            Base Green 3

        "Blue 1" ->
            Base Blue 1

        "Blue 2" ->
            Base Blue 2

        "Blue 3" ->
            Base Blue 3

        "Black 1" ->
            Base Black 1

        "Black 2" ->
            Base Black 2

        "Black 3" ->
            Base Black 3

        "Purple" ->
            Combined Purple

        "White" ->
            Combined White

        "Pink" ->
            Combined Pink

        "Yellow" ->
            Combined Yellow

        "Orange" ->
            Combined Orange

        "Teal" ->
            Combined Teal

        "DarkBlue" ->
            Combined DarkBlue

        "Grey" ->
            Combined Grey

        "Gold" ->
            Combined Gold

        _ ->
            Base Green 1


getTowerType : Seed -> List Int -> ( Seed, TowerType )
getTowerType seed chances =
    let
        splitInFour value =
            List.repeat (List.length colors) (value // List.length colors)

        chancesBuildUp =
            List.foldl
                (\value acc ->
                    let
                        from =
                            List.Extra.last acc
                                |> Maybe.map Tuple.second
                                |> Maybe.withDefault 0
                    in
                    acc ++ [ ( from, from + value ) ]
                )
                []
                (List.foldl (\chance acc -> acc ++ splitInFour chance) [] chances)

        ( random, newSeed ) =
            Random.step (Random.int 0 (List.sum chances)) seed

        sortTowers towerType =
            case towerType of
                Base _ int ->
                    int

                Combined _ ->
                    0

        chanceList =
            List.Extra.zip chancesBuildUp (List.sortBy sortTowers basicTowers)
    in
    ( newSeed
    , chanceList
        |> List.Extra.find (\( ( from, to ), _ ) -> from <= random && random < to)
        |> Maybe.map Tuple.second
        |> Maybe.withDefault (Base Green 1)
    )


combinedTowerTypeString : CombinedTower -> String
combinedTowerTypeString combinedTowerType =
    case combinedTowerType of
        Purple ->
            "Purple"

        White ->
            "White"

        Pink ->
            "Pink"

        Yellow ->
            "Yellow"

        Orange ->
            "Orange"

        Teal ->
            "Teal"

        Grey ->
            "Grey"

        DarkBlue ->
            "Dark blue"

        Gold ->
            "Gold"


towerCombination : TowerType -> List TowerType
towerCombination towerType =
    case towerType of
        Combined White ->
            [ Base Blue 1, Base Red 1, Base Green 1 ]

        Combined Teal ->
            [ Base Blue 1, Base Black 1, Base Green 1 ]

        Combined Purple ->
            [ Base Blue 2, Base Black 2, Base Red 2 ]

        Combined Orange ->
            [ Base Red 1, Base Red 2, Base Green 2 ]

        Combined Grey ->
            [ Base Black 1, Base Black 2, Combined White ]

        Combined DarkBlue ->
            [ Base Blue 2, Base Blue 3, Base Black 3 ]

        Combined Pink ->
            [ Base Red 3, Combined White, Combined Purple ]

        Combined Yellow ->
            [ Combined White, Combined Orange, Base Green 3 ]

        Combined Gold ->
            [ Combined Yellow, Base Black 1, Base Black 2 ]

        Base _ _ ->
            []


towerCombinations : List ( TowerType, List TowerType )
towerCombinations =
    List.map
        (\towerType ->
            ( towerType
            , towerCombination towerType
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
            "Flying damage " ++ String.fromFloat (extra * 100) ++ "%"

        TrueStrike ->
            "Ignore evasion"


viewTowerInformation : List TowerType -> List TowerType -> Html msg
viewTowerInformation temporaryTowerTypes existingTowerTypes =
    let
        haveTemporarily towerType =
            List.member towerType temporaryTowerTypes

        have towerType =
            List.member towerType existingTowerTypes

        towerBlock : TowerType -> Html msg
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
                    towerCombination towerType
            in
            div [ class "tower-block", class (towerInfoClass (haveTemporarily towerType) (have towerType)) ]
                [ div [ class "tower-block-inner" ]
                    [ div [ class "tower-image" ]
                        [ viewTower False tower
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
                , div
                    [ class "special-text" ]
                    [ text (tower.effects |> List.map effectString |> String.join ", ")
                    ]
                , div [ class "tower-images" ]
                    (List.map
                        (\tt ->
                            div
                                [ class "tower-image"
                                , class
                                    (towerInfoClass (haveTemporarily tt) (have tt))
                                ]
                                [ viewTower False (createTower tt False 0) ]
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
    if have then
        "have"

    else if haveTemporarily then
        "have-temporarily"

    else
        ""


createTower : TowerType -> Bool -> Int -> Tower
createTower towerType temporary cellIndex =
    let
        values =
            case towerType of
                Base Red 1 ->
                    { range = 80, damage = 6, rate = 100, effects = [ SpeedAura 10 ], targets = 1 }

                Base Red 2 ->
                    { range = 80, damage = 12, rate = 100, effects = [ SpeedAura 20 ], targets = 1 }

                Base Red 3 ->
                    { range = 80, damage = 36, rate = 100, effects = [ SpeedAura 30 ], targets = 1 }

                Base Green 1 ->
                    { range = 100, damage = 15, rate = 100, effects = [], targets = 1 }

                Base Green 2 ->
                    { range = 100, damage = 30, rate = 100, effects = [], targets = 1 }

                Base Green 3 ->
                    { range = 100, damage = 90, rate = 100, effects = [], targets = 1 }

                Base Blue 1 ->
                    { range = 125, damage = 10, rate = 100, effects = [ SlowEffect 10 ], targets = 1 }

                Base Blue 2 ->
                    { range = 125, damage = 20, rate = 100, effects = [ SlowEffect 20 ], targets = 1 }

                Base Blue 3 ->
                    { range = 125, damage = 60, rate = 100, effects = [ SlowEffect 30 ], targets = 1 }

                Base Black 1 ->
                    { range = 100, damage = 5, rate = 100, effects = [], targets = 2 }

                Base Black 2 ->
                    { range = 100, damage = 10, rate = 100, effects = [], targets = 3 }

                Base Black 3 ->
                    { range = 100, damage = 30, rate = 100, effects = [], targets = 4 }

                Combined White ->
                    { range = 150, damage = 35, rate = 140, effects = [], targets = 1 }

                Combined Teal ->
                    { range = 150, damage = 23, rate = 140, effects = [], targets = 3 }

                Combined Orange ->
                    { range = 100, damage = 65, rate = 120, effects = [ SpeedAura 20 ], targets = 1 }

                Combined Purple ->
                    { range = 150, damage = 85, rate = 110, effects = [ TrueStrike ], targets = 1 }

                Combined Grey ->
                    { range = 150, damage = 50, rate = 100, effects = [ FlyingDamage 1.8 ], targets = 5 }

                Combined DarkBlue ->
                    { range = 85, damage = 40, rate = 60, effects = [ SlowEffect 75 ], targets = 10 }

                Combined Pink ->
                    { range = 180, damage = 115, rate = 125, effects = [ TrueStrike, SpeedAura 40 ], targets = 1 }

                Combined Yellow ->
                    { range = 180, damage = 180, rate = 160, effects = [], targets = 1 }

                Combined Gold ->
                    { range = 180, damage = 180, rate = 160, effects = [], targets = 4 }

                _ ->
                    { range = 100, damage = 100, rate = 100, effects = [], targets = 1 }
    in
    { damage = values.damage
    , totalDamage = 0
    , range = values.range
    , cellIndex = cellIndex
    , rate = values.rate
    , cooldown = 0
    , targets = values.targets
    , temporary = temporary
    , towerType = towerType
    , effects = values.effects
    }


viewTower : Bool -> Tower -> Html msg
viewTower selected tower =
    let
        bar color index =
            div
                [ class "bar"
                , style "bottom" (intToPxString ((index * 4) - 1))
                , style "background-color" (colorToCssString color)
                ]
                []

        bars =
            case tower.towerType of
                Base color int ->
                    List.map (bar color) (List.range 1 int)

                Combined _ ->
                    []

        blockHelper color =
            div
                [ class "block"
                , style "background-color" color
                ]
                []

        block =
            case tower.towerType of
                Combined _ ->
                    blockHelper (towerTypeToCssString tower.towerType)

                Base _ _ ->
                    text ""
    in
    div
        [ class "tower"
        , class
            (if selected then
                "selected"

             else
                ""
            )
        , class
            (if tower.temporary then
                "temporary"

             else
                ""
            )
        ]
        ([]
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
                && List.member forTower existingTowerType
    in
    towerCombinations
        |> List.filter buildable
        |> List.map Tuple.first
