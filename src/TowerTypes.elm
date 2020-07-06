module TowerTypes exposing (..)


type alias Tower =
    { name : String
    , color : String
    , damage : Int
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
    case string of
        "Red 1" ->
            Red1

        "Red 2" ->
            Red2

        "Red 3" ->
            Red3

        "Green 1" ->
            Green1

        "Green 2" ->
            Green2

        "Green 3" ->
            Green3

        "Blue 1" ->
            Blue1

        "Blue 2" ->
            Blue2

        "Blue 3" ->
            Blue3

        "Black 1" ->
            Black1

        "Black 2" ->
            Black2

        "Black 3" ->
            Black3

        "Purple" ->
            Purple

        "White" ->
            White

        "Pink" ->
            Pink

        "Yellow" ->
            Yellow

        "Orange" ->
            Orange

        "Teal" ->
            Teal

        "DarkBlue" ->
            DarkBlue

        "Grey" ->
            Grey

        "Gold" ->
            Gold

        _ ->
            Green1
