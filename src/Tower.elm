module Tower exposing (..)

import Constants exposing (towerSize)
import Helper exposing (intToPxString)
import Html exposing (Html, div, h1, h3, span, text)
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


type TowerType
    = Base Color Int
    | Combined CombinedTower


basicTowers : List TowerType
basicTowers =
    List.foldl (++) [] (List.map (\color -> List.map (Base color) (List.range 1 maxTowerLevel)) colors)


combinedTowers : List TowerType
combinedTowers =
    [ Combined Purple ]


colorToString : Color -> String
colorToString color =
    case color of
        Green ->
            "Green"

        Red ->
            "Red"

        Blue ->
            "Blue"


projectileColor : TowerType -> String
projectileColor towerType =
    case towerType of
        Base color _ ->
            colorToString color

        Combined Purple ->
            "purple"


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
            [ 100, 0, 0, 0 ]

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


towerCombination : TowerType -> List TowerType
towerCombination towerType =
    case towerType of
        Combined Purple ->
            [ Base Blue 1, Base Red 1 ]

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
    in
    div []
        ([ h3 [] [ text "Base towers" ] ]
            ++ List.map
                (\towerType ->
                    viewBaseTower
                        (haveTemporarily towerType)
                        (have towerType)
                        towerType
                )
                basicTowers
            ++ [ h3 [] [ text "Combined towers" ] ]
            ++ List.map
                (\( towerType, combinations ) ->
                    case towerType of
                        Combined Purple ->
                            viewTowerCombination
                                temporaryTowerTypes
                                existingTowerTypes
                                ( towerType, combinations )

                        Base _ _ ->
                            div [] []
                )
                towerCombinations
        )


towerInfoColor : Bool -> Bool -> String
towerInfoColor haveTemporarily have =
    if have then
        "green"

    else if haveTemporarily then
        "orange"

    else
        "black"


viewBaseTower : Bool -> Bool -> TowerType -> Html msg
viewBaseTower haveTemporarily have towerType =
    span
        [ style "margin-right" "5px"
        , style "color" (towerInfoColor haveTemporarily have)
        ]
        [ text (towerTypeString towerType) ]


viewTowerCombination : List TowerType -> List TowerType -> ( TowerType, List TowerType ) -> Html msg
viewTowerCombination temporaryTowerTypes existingTowerTypes ( towerType, combinations ) =
    let
        haveTemporarily t =
            List.member t temporaryTowerTypes

        have t =
            List.member t existingTowerTypes
    in
    div []
        ([ span [ style "margin-right" "5px" ] [ text (towerTypeString towerType ++ ":") ] ]
            ++ List.map
                (\combinationTowerType ->
                    span
                        [ style "margin-right" "5px"
                        , style "color"
                            (towerInfoColor
                                (haveTemporarily combinationTowerType)
                                (have combinationTowerType)
                            )
                        ]
                        [ text (towerTypeString combinationTowerType) ]
                )
                combinations
        )


type alias TowerValues =
    { range : Int
    , damage : Int
    , cooldown : Int
    , targets : Int
    }


createTower : TowerType -> Int -> Tower
createTower towerType cellIndex =
    let
        values =
            case towerType of
                Base Red 1 ->
                    { range = 50, damage = 10, cooldown = 7, targets = 1 }

                Base Red 2 ->
                    { range = 1, damage = 1, cooldown = 1, targets = 1 }

                Base Red 3 ->
                    { range = 1, damage = 1, cooldown = 1, targets = 1 }

                Base Red _ ->
                    { range = 1, damage = 1, cooldown = 1, targets = 1 }

                Base Green 1 ->
                    { range = 200, damage = 4, cooldown = 3, targets = 3 }

                Base Green 2 ->
                    { range = 1, damage = 1, cooldown = 1, targets = 1 }

                Base Green 3 ->
                    { range = 1, damage = 1, cooldown = 1, targets = 1 }

                Base Green _ ->
                    { range = 1, damage = 1, cooldown = 1, targets = 1 }

                Base Blue 1 ->
                    { range = 100, damage = 5, cooldown = 5, targets = 1 }

                Base Blue 2 ->
                    { range = 1, damage = 1, cooldown = 1, targets = 1 }

                Base Blue 3 ->
                    { range = 1, damage = 1, cooldown = 1, targets = 1 }

                Base Blue _ ->
                    { range = 1, damage = 1, cooldown = 1, targets = 1 }

                Combined Purple ->
                    { range = 100, damage = 100, cooldown = 1, targets = 10 }
    in
    { damage = values.damage
    , totalDamage = 0
    , range = values.range
    , cellIndex = cellIndex
    , cooldown = values.cooldown
    , currentCooldown = 0
    , targets = values.targets
    , temporary = True
    , towerType = towerType
    }


viewTower : Bool -> Tower -> Html msg
viewTower selected tower =
    div
        [ class "tower"
        , style "width" (intToPxString towerSize)
        , style "height" (intToPxString towerSize)
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
        [ div [ class "tower-inner" ] (towerContent tower.towerType)
        , if selected then
            div
                [ class "tower-range"
                , style "width" (intToPxString (tower.range * 2))
                , style "height" (intToPxString (tower.range * 2))
                , style "top" (intToPxString (towerSize // 2 - ((tower.range * 2) // 2)))
                , style "left" (intToPxString (towerSize // 2 - ((tower.range * 2) // 2)))
                ]
                []

          else
            text ""
        ]


towerContent : TowerType -> List (Html msg)
towerContent towerType =
    let
        bar color =
            div [ class "bar", style "background" color ] []

        box color =
            div [ class "box", style "background" color ] []
    in
    case towerType of
        Base color level ->
            let
                colorString =
                    colorToString color
            in
            List.map (\_ -> bar colorString) (List.range 1 level)

        Combined combinedTower ->
            case combinedTower of
                Purple ->
                    [ box "purple" ]


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
