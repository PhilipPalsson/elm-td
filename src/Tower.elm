module Tower exposing (..)

import Constants exposing (cellSize, fps)
import Helper exposing (intToPxString)
import Html exposing (Html, div, h3, table, td, text, th, tr)
import Html.Attributes exposing (class, style)
import List.Extra
import Random exposing (Seed)


type alias Tower =
    { damage : Int
    , flyingDamage : Float
    , totalDamage : Int
    , range : Int
    , cellIndex : Int
    , cooldown : Int
    , currentCooldown : Int
    , targets : Int
    , temporary : Bool
    , towerType : TowerType
    }


type alias TowerId =
    Int


type Color
    = Green
    | Red
    | Blue


colors =
    [ Red, Green, Blue ]


maxTowerLevel =
    3


type CombinedTower
    = Teal
    | White
    | Purple
    | Orange
    | BigGreen
    | Pink
    | Yellow


type TowerType
    = Base Color Int
    | Combined CombinedTower


basicTowers : List TowerType
basicTowers =
    List.foldl (++) [] (List.map (\color -> List.map (Base color) (List.range 1 maxTowerLevel)) colors)


combinedTowers : List TowerType
combinedTowers =
    [ Combined White
    , Combined Teal
    , Combined Purple
    , Combined Orange
    , Combined BigGreen
    , Combined Pink
    , Combined Yellow
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


colorToString : Color -> String
colorToString color =
    case color of
        Green ->
            "Green"

        Red ->
            "Red"

        Blue ->
            "Blue"


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

        Combined BigGreen ->
            "green"


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

        "BigGreen" ->
            Combined BigGreen

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

        BigGreen ->
            "Big Green"


towerCombination : TowerType -> List TowerType
towerCombination towerType =
    case towerType of
        Combined Teal ->
            [ Base Blue 1, Base Blue 2, Base Green 1 ]

        Combined White ->
            [ Base Blue 1, Base Red 1, Base Green 1 ]

        Combined Purple ->
            [ Base Blue 2, Base Blue 3, Base Red 2 ]

        Combined Orange ->
            [ Base Red 1, Base Red 2, Base Green 2 ]

        Combined BigGreen ->
            [ Base Green 1, Base Green 2, Base Green 3 ]

        Combined Pink ->
            [ Base Red 3, Combined White, Combined Purple ]

        Combined Yellow ->
            [ Combined White, Combined Orange, Combined BigGreen ]

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
                    toFloat fps / toFloat tower.cooldown

                rate =
                    round (hitsPerSecond * 100)

                maxDps =
                    hitsPerSecond * toFloat tower.damage * toFloat tower.targets

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
                            , th [] [ text "Max dps" ]
                            ]
                        , tr []
                            [ td [] [ text (String.fromInt tower.damage) ]
                            , td [] [ text (String.fromInt tower.targets) ]
                            , td [] [ text (String.fromInt tower.range) ]
                            , td [] [ text (String.fromInt rate) ]
                            , td [] [ text (String.fromInt (round maxDps)) ]
                            ]
                        ]
                    ]
                , if tower.flyingDamage > 1 then
                    div
                        [ class "special-text" ]
                        [ text
                            ("Special: Flying damage "
                                ++ String.fromFloat (tower.flyingDamage * 100)
                                ++ "%"
                            )
                        ]

                  else
                    div [] []
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
                    { range = 200, damage = 7, flyingDamage = 1, cooldown = fps, targets = 1 }

                Base Red 2 ->
                    { range = 200, damage = 12, flyingDamage = 1.5, cooldown = fps, targets = 1 }

                Base Red 3 ->
                    { range = 200, damage = 50, flyingDamage = 2, cooldown = fps, targets = 1 }

                Base Green 1 ->
                    { range = 80, damage = 12, flyingDamage = 1, cooldown = fps, targets = 1 }

                Base Green 2 ->
                    { range = 90, damage = 20, flyingDamage = 1, cooldown = fps, targets = 1 }

                Base Green 3 ->
                    { range = 100, damage = 80, flyingDamage = 1, cooldown = fps, targets = 1 }

                Base Blue 1 ->
                    { range = 100, damage = 5, flyingDamage = 1, cooldown = fps, targets = 2 }

                Base Blue 2 ->
                    { range = 100, damage = 8, flyingDamage = 1, cooldown = fps, targets = 3 }

                Base Blue 3 ->
                    { range = 100, damage = 35, flyingDamage = 1, cooldown = fps, targets = 4 }

                Combined Teal ->
                    { range = 100, damage = 18, flyingDamage = 1, cooldown = fps, targets = 4 }

                Combined White ->
                    { range = 150, damage = 25, flyingDamage = 1, cooldown = fps, targets = 1 }

                Combined Purple ->
                    { range = 100, damage = 60, flyingDamage = 1, cooldown = fps, targets = 5 }

                Combined Orange ->
                    { range = 220, damage = 45, flyingDamage = 1, cooldown = fps, targets = 1 }

                Combined BigGreen ->
                    { range = 100, damage = 100, flyingDamage = 1, cooldown = fps, targets = 1 }

                Combined Pink ->
                    { range = 180, damage = 90, flyingDamage = 1, cooldown = fps, targets = 8 }

                Combined Yellow ->
                    { range = 180, damage = 180, flyingDamage = 1, cooldown = fps, targets = 1 }

                _ ->
                    { range = 100, damage = 100, flyingDamage = 1, cooldown = fps, targets = 1 }
    in
    { damage = values.damage
    , flyingDamage = values.flyingDamage
    , totalDamage = 0
    , range = values.range
    , cellIndex = cellIndex
    , cooldown = values.cooldown
    , currentCooldown = 0
    , targets = values.targets
    , temporary = temporary
    , towerType = towerType
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
