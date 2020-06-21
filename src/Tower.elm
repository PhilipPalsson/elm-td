module Tower exposing (..)

import Constants exposing (cellSize)
import Helper exposing (intToPxString)
import Html exposing (Html, div, h3, span, table, td, text, th, tr)
import Html.Attributes exposing (class, style)
import List.Extra
import Random exposing (Seed)


type alias Tower =
    { damage : Int
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
    4


type CombinedTower
    = Purple
    | White
    | Pink
    | Yellow
    | Orange
    | BigGreen
    | BigRed
    | BigBlue


type TowerType
    = Base Color Int
    | Combined CombinedTower


basicTowers : List TowerType
basicTowers =
    List.foldl (++) [] (List.map (\color -> List.map (Base color) (List.range 1 maxTowerLevel)) colors)


combinedTowers : List TowerType
combinedTowers =
    [ Combined Purple
    , Combined White
    , Combined Pink
    , Combined Yellow
    , Combined Orange
    , Combined BigGreen
    , Combined BigRed
    , Combined BigBlue
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

        Combined BigGreen ->
            "green"

        Combined BigRed ->
            "red"

        Combined BigBlue ->
            "blue"


towerTypeString : TowerType -> String
towerTypeString towerType =
    case towerType of
        Base color level ->
            colorToString color ++ " " ++ String.fromInt level

        Combined combinedTower ->
            combinedTowerTypeString combinedTower


getTowerType : Seed -> ( Seed, TowerType )
getTowerType seed =
    let
        chances =
            [ 25, 25, 25, 25 ]

        --[ 60, 20, 12, 8 ]
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

        BigGreen ->
            "Big Green"

        BigRed ->
            "Big Red"

        BigBlue ->
            "Big Blue"


towerCombination : TowerType -> List TowerType
towerCombination towerType =
    case towerType of
        Combined Purple ->
            [ Base Blue 1, Base Red 1, Base Red 2 ]

        Combined White ->
            [ Base Blue 1, Base Red 1, Base Green 1 ]

        Combined Pink ->
            [ Combined White, Base Red 1, Combined Purple ]

        Combined Yellow ->
            [ Base Green 1, Base Green 2, Base Red 1 ]

        Combined Orange ->
            [ Combined Yellow, Base Red 1, Base Red 2 ]

        Combined BigGreen ->
            [ Base Green 1, Base Green 2, Base Green 3 ]

        Combined BigRed ->
            [ Base Red 1, Base Red 2, Base Red 3 ]

        Combined BigBlue ->
            [ Base Blue 1, Base Blue 2, Base Blue 3 ]

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

        baseTower : TowerType -> Html msg
        baseTower towerType =
            let
                tower =
                    createTower towerType False 0
            in
            div [ class "tower-block", class (towerInfoClass (haveTemporarily towerType) (have towerType)) ]
                [ div [ class "tower-image" ]
                    [ viewTower False tower
                    ]
                , table [ class "tower-info" ]
                    [ tr []
                        [ th [] [ text "Damage" ]
                        , th [] [ text "Targets" ]
                        , th [] [ text "Range" ]
                        , th [] [ text "Cooldown" ]
                        , th [] [ text "Max dps" ]
                        ]
                    , tr []
                        [ td [] [ text (String.fromInt tower.damage) ]
                        , td [] [ text (String.fromInt tower.targets) ]
                        , td [] [ text (String.fromInt tower.range) ]
                        , td [] [ text (String.fromInt tower.cooldown) ]
                        , td [] [ text (String.fromInt (round (toFloat tower.damage * (1 / toFloat tower.cooldown)))) ]
                        ]
                    ]
                ]

        combinedTower : TowerType -> Html msg
        combinedTower towerType =
            let
                tower =
                    createTower towerType False 0

                combinations =
                    towerCombination towerType
            in
            div [ class "tower-block tower-block-combined", class (towerInfoClass (haveTemporarily towerType) (have towerType)) ]
                [ div [ class "tower-block-inner" ]
                    [ div [ class "tower-image" ]
                        [ viewTower False tower
                        ]
                    , table [ class "tower-info" ]
                        [ tr []
                            [ th [] [ text "Damage" ]
                            , th [] [ text "Targets" ]
                            , th [] [ text "Range" ]
                            , th [] [ text "Cooldown" ]
                            , th [] [ text "Max dps" ]
                            ]
                        , tr []
                            [ td [] [ text (String.fromInt tower.damage) ]
                            , td [] [ text (String.fromInt tower.targets) ]
                            , td [] [ text (String.fromInt tower.range) ]
                            , td [] [ text (String.fromInt tower.cooldown) ]
                            , td [] [ text (String.fromInt (round (toFloat tower.damage * (1 / toFloat tower.cooldown)))) ]
                            ]
                        ]
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
        , div [ class "tower-list" ] (List.map baseTower basicTowers)
        , h3 []
            [ text "Combined towers"
            ]
        , div [ class "tower-list" ] (List.map combinedTower combinedTowers)
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
                    { range = 50, damage = 10, cooldown = 7, targets = 1 }

                Base Red 2 ->
                    { range = 100, damage = 1, cooldown = 1, targets = 1 }

                Base Red 3 ->
                    { range = 100, damage = 1, cooldown = 1, targets = 1 }

                Base Red _ ->
                    { range = 100, damage = 1, cooldown = 1, targets = 1 }

                Base Green 1 ->
                    { range = 200, damage = 4, cooldown = 3, targets = 3 }

                Base Green 2 ->
                    { range = 100, damage = 1, cooldown = 1, targets = 1 }

                Base Green 3 ->
                    { range = 100, damage = 1, cooldown = 1, targets = 1 }

                Base Green _ ->
                    { range = 100, damage = 1, cooldown = 1, targets = 1 }

                Base Blue 1 ->
                    { range = 10000, damage = 5, cooldown = 5, targets = 1 }

                Base Blue 2 ->
                    { range = 100, damage = 1, cooldown = 1, targets = 1 }

                Base Blue 3 ->
                    { range = 100, damage = 1, cooldown = 1, targets = 1 }

                Base Blue _ ->
                    { range = 100, damage = 1, cooldown = 1, targets = 1 }

                Combined Purple ->
                    { range = 100, damage = 100, cooldown = 1, targets = 10 }

                Combined White ->
                    { range = 100, damage = 100, cooldown = 1, targets = 10 }

                Combined Pink ->
                    { range = 100, damage = 100, cooldown = 1, targets = 10 }

                Combined Yellow ->
                    { range = 100, damage = 100, cooldown = 1, targets = 10 }

                Combined Orange ->
                    { range = 100, damage = 100, cooldown = 1, targets = 10 }

                Combined BigGreen ->
                    { range = 100, damage = 100, cooldown = 1, targets = 10 }

                Combined BigRed ->
                    { range = 100, damage = 100, cooldown = 1, targets = 10 }

                Combined BigBlue ->
                    { range = 100, damage = 100, cooldown = 1, targets = 10 }
    in
    { damage = values.damage
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

                Base color int ->
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
