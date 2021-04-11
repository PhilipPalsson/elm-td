module Tower exposing
    ( Tower
    , TowerData
    , TowerEffect(..)
    , TowerId
    , TowerType(..)
    , basicTowers
    , codec
    , combinedTowers
    , getTowerData
    )

import List.Extra
import Serialize as S exposing (Codec)


type alias Tower =
    { totalDamage : Int
    , cellIndex : Int
    , cooldown : Int
    , temporary : Bool
    , towerType : TowerType
    }


type alias TowerData =
    { name : String
    , range : Int
    , damage : Int
    , attackRate : Int
    , effects : List TowerEffect
    , maximumTargets : Int
    , color : String
    , combinations : List TowerType
    }


type alias TowerId =
    Int


type TowerType
    = Red1
    | Red2
    | Red3
    | Green1
    | Green2
    | Green3
    | Blue1
    | Blue2
    | Blue3
    | Black1
    | Black2
    | Black3
    | White
    | Teal
    | Purple
    | Orange
    | Grey
    | DarkBlue
    | Pink
    | Yellow
    | Gold


type TowerEffect
    = FlyingDamage Int
    | SpeedAura Int
    | SlowEffect Int
    | TrueStrike


towerTypeFromString : String -> TowerType
towerTypeFromString string =
    (basicTowers ++ combinedTowers)
        |> List.Extra.find (\t -> (getTowerData t).name == string)
        |> Maybe.withDefault Red1


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


codec : Codec e Tower
codec =
    S.record Tower
        |> S.field .totalDamage S.int
        |> S.field .cellIndex S.int
        |> S.field .cooldown S.int
        |> S.field .temporary S.bool
        |> S.field .towerType (S.string |> S.map towerTypeFromString (getTowerData >> .name))
        |> S.finishRecord


getTowerData : TowerType -> TowerData
getTowerData towerType =
    case towerType of
        Red1 ->
            { name = "Red 1"
            , range = 100
            , damage = 6
            , attackRate = 100
            , effects = [ SpeedAura 10 ]
            , maximumTargets = 1
            , color = "red"
            , combinations = []
            }

        Red2 ->
            { name = "Red 2"
            , range = 100
            , damage = 12
            , attackRate = 100
            , effects = [ SpeedAura 20 ]
            , maximumTargets = 1
            , color = "red"
            , combinations = []
            }

        Red3 ->
            { name = "Red 3"
            , range = 100
            , damage = 36
            , attackRate = 100
            , effects = [ SpeedAura 30 ]
            , maximumTargets = 1
            , color = "red"
            , combinations = []
            }

        Green1 ->
            { name = "Green 1"
            , range = 110
            , damage = 15
            , attackRate = 100
            , effects = []
            , maximumTargets = 1
            , color = "#19e219"
            , combinations = []
            }

        Green2 ->
            { name = "Green 2"
            , range = 110
            , damage = 30
            , attackRate = 100
            , effects = []
            , maximumTargets = 1
            , color = "#19e219"
            , combinations = []
            }

        Green3 ->
            { name = "Green 3"
            , range = 110
            , damage = 90
            , attackRate = 100
            , effects = []
            , maximumTargets = 1
            , color = "#19e219"
            , combinations = []
            }

        Blue1 ->
            { name = "Blue 1"
            , range = 130
            , damage = 8
            , attackRate = 100
            , effects = [ SlowEffect 10 ]
            , maximumTargets = 1
            , color = "#008eff"
            , combinations = []
            }

        Blue2 ->
            { name = "Blue 2"
            , range = 130
            , damage = 16
            , attackRate = 100
            , effects = [ SlowEffect 20 ]
            , maximumTargets = 1
            , color = "#008eff"
            , combinations = []
            }

        Blue3 ->
            { name = "Blue 3"
            , range = 130
            , damage = 48
            , attackRate = 100
            , effects = [ SlowEffect 30 ]
            , maximumTargets = 1
            , color = "#008eff"
            , combinations = []
            }

        Black1 ->
            { name = "Black 1"
            , range = 110
            , damage = 5
            , attackRate = 100
            , effects = []
            , maximumTargets = 2
            , color = "black"
            , combinations = []
            }

        Black2 ->
            { name = "Black 2"
            , range = 110
            , damage = 10
            , attackRate = 100
            , effects = []
            , maximumTargets = 3
            , color = "black"
            , combinations = []
            }

        Black3 ->
            { name = "Black 3"
            , range = 110
            , damage = 30
            , attackRate = 100
            , effects = []
            , maximumTargets = 4
            , color = "black"
            , combinations = []
            }

        White ->
            { name = "White"
            , range = 125
            , damage = 33
            , attackRate = 115
            , effects = []
            , maximumTargets = 1
            , color = "white"
            , combinations = [ Blue1, Red1, Green1 ]
            }

        Teal ->
            { name = "Teal"
            , range = 125
            , damage = 23
            , attackRate = 120
            , effects = [ SlowEffect 10 ]
            , maximumTargets = 3
            , color = "teal"
            , combinations = [ Blue1, Black1, Green1 ]
            }

        Orange ->
            { name = "Orange"
            , range = 100
            , damage = 60
            , attackRate = 120
            , effects = [ SpeedAura 20 ]
            , maximumTargets = 1
            , color = "orange"
            , combinations = [ Red1, Red2, Green2 ]
            }

        Purple ->
            { name = "Purple"
            , range = 125
            , damage = 80
            , attackRate = 110
            , effects = [ TrueStrike ]
            , maximumTargets = 1
            , color = "purple"
            , combinations = [ Blue2, Black2, Red2 ]
            }

        Grey ->
            { name = "Grey"
            , range = 125
            , damage = 42
            , attackRate = 140
            , effects = [ FlyingDamage 200 ]
            , maximumTargets = 4
            , color = "darkgrey"
            , combinations = [ Black1, Black2, White ]
            }

        DarkBlue ->
            { name = "Dark Blue"
            , range = 85
            , damage = 40
            , attackRate = 60
            , effects = [ SlowEffect 50 ]
            , maximumTargets = 10
            , color = "darkblue"
            , combinations = [ Black2, Blue2, Blue3 ]
            }

        Pink ->
            { name = "Pink"
            , range = 180
            , damage = 105
            , attackRate = 130
            , effects = [ TrueStrike, SpeedAura 40 ]
            , maximumTargets = 1
            , color = "pink"
            , combinations = [ Red3, White, Purple ]
            }

        Yellow ->
            { name = "Yellow"
            , range = 150
            , damage = 155
            , attackRate = 140
            , effects = []
            , maximumTargets = 1
            , color = "yellow"
            , combinations = [ Green3, White, Orange ]
            }

        Gold ->
            { name = "Gold"
            , range = 160
            , damage = 155
            , attackRate = 145
            , effects = []
            , maximumTargets = 4
            , color = "gold"
            , combinations = [ Black1, Black3, Yellow ]
            }
