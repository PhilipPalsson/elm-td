module Types exposing
    ( Board
    , Cell
    , CellIndex
    , CellObject(..)
    , CellType(..)
    , Enemy
    , EnemyId
    , GameModel
    , GameState(..)
    , Position
    , Projectile
    , Projectiles
    , Selected(..)
    , Towers
    , gameModelDecoder
    , gameModelEncoder
    )

import Array exposing (Array)
import Dict exposing (Dict)
import Json.Decode as Decode
import Json.Decode.Extra as Decode
import Json.Encode as Encode
import Random exposing (Seed)
import Tower exposing (Tower, TowerId, TowerType, towerTypeFromString, towerTypeString)


type GameState
    = Level
    | Build Int
    | GameOver
    | Paused


type Selected
    = TowerSelected TowerId
    | EnemySelected EnemyId
    | StoneSelected CellIndex
    | NothingSelected


type alias Projectile =
    { enemyId : EnemyId, from : Position, ttl : Int, color : String }


type alias Projectiles =
    List Projectile


type alias GameModel =
    { board : Board
    , enemies : List Enemy
    , enemyIdCount : EnemyId
    , state : GameState
    , selected : Selected
    , towerIdCount : TowerId
    , towers : Towers
    , projectiles : Projectiles
    , hp : Int
    , level : Int
    , seed : Seed
    }


type alias EnemyId =
    Int


type alias Towers =
    Dict TowerId Tower


type CellObject
    = CellTower TowerId
    | Stone
    | Blocked
    | NoCellObject


type CellType
    = Path CellObject
    | Grass CellObject
    | Start
    | Goal
    | Post


type alias Cell =
    { cellType : CellType, index : CellIndex }


type alias Position =
    { x : Int, y : Int }


type alias Enemy =
    { position : Position
    , path : List Position
    , id : Int
    , hp : Int
    , maxHp : Int
    , damage : Int
    , spawnTime : Int
    , flying : Bool
    , boss : Bool
    }


type alias Board =
    Array Cell


type alias CellIndex =
    Int


cellObjectEncoder : CellObject -> Encode.Value
cellObjectEncoder cellObject =
    Encode.string
        (case cellObject of
            CellTower towerId ->
                "Tower_" ++ String.fromInt towerId

            Stone ->
                "Stone"

            Blocked ->
                "Blocked"

            NoCellObject ->
                "NoCellObject"
        )


cellTypeEncoder : CellType -> Encode.Value
cellTypeEncoder cellType =
    case cellType of
        Path cellObject ->
            Encode.object
                [ ( "type", Encode.string "Path" )
                , ( "cellObject", cellObjectEncoder cellObject )
                ]

        Grass cellObject ->
            Encode.object
                [ ( "type", Encode.string "Grass" )
                , ( "cellObject", cellObjectEncoder cellObject )
                ]

        Start ->
            Encode.object
                [ ( "type", Encode.string "Start" )
                , ( "cellObject", Encode.null )
                ]

        Goal ->
            Encode.object
                [ ( "type", Encode.string "Goal" )
                , ( "cellObject", Encode.null )
                ]

        Post ->
            Encode.object
                [ ( "type", Encode.string "Post" )
                , ( "cellObject", Encode.null )
                ]


cellEncoder : Cell -> Encode.Value
cellEncoder cell =
    Encode.object
        [ ( "cellType", cellTypeEncoder cell.cellType )
        , ( "index", Encode.int cell.index )
        ]


positionEncoder : Position -> Encode.Value
positionEncoder position =
    Encode.object
        [ ( "x", Encode.int position.x )
        , ( "y", Encode.int position.y )
        ]


enemyEncoder : Enemy -> Encode.Value
enemyEncoder enemy =
    Encode.object
        [ ( "position", positionEncoder enemy.position )
        , ( "path", Encode.list positionEncoder enemy.path )
        , ( "id", Encode.int enemy.id )
        , ( "hp", Encode.int enemy.hp )
        , ( "maxHp", Encode.int enemy.maxHp )
        , ( "damage", Encode.int enemy.damage )
        , ( "spawnTime", Encode.int enemy.spawnTime )
        , ( "flying", Encode.bool enemy.flying )
        , ( "boss", Encode.bool enemy.boss )
        ]


gameStateEncoder : GameState -> Encode.Value
gameStateEncoder gameState =
    Encode.string
        (case gameState of
            Level ->
                "Level"

            Build int ->
                "Build_" ++ String.fromInt int

            GameOver ->
                "GameOver"

            Paused ->
                "Paused"
        )


selectedEncoder : Selected -> Encode.Value
selectedEncoder selected =
    case selected of
        TowerSelected towerId ->
            Encode.object
                [ ( "selectedType", Encode.string "Tower" )
                , ( "selectedId", Encode.int towerId )
                ]

        EnemySelected enemyId ->
            Encode.object
                [ ( "selectedType", Encode.string "Enemy" )
                , ( "selectedId", Encode.int enemyId )
                ]

        StoneSelected cellIndex ->
            Encode.object
                [ ( "selectedType", Encode.string "Stone" )
                , ( "selectedId", Encode.int cellIndex )
                ]

        NothingSelected ->
            Encode.object
                [ ( "selectedType", Encode.string "Nothing" )
                , ( "selectedId", Encode.int 0 )
                ]


towerTypeEncoder : TowerType -> Encode.Value
towerTypeEncoder towerType =
    Encode.string (towerTypeString towerType)


towerEncoder : Tower -> Encode.Value
towerEncoder tower =
    Encode.object
        [ ( "damage", Encode.int tower.damage )
        , ( "flyingDamage", Encode.float tower.flyingDamage )
        , ( "totalDamage", Encode.int tower.totalDamage )
        , ( "range", Encode.int tower.range )
        , ( "cellIndex", Encode.int tower.cellIndex )
        , ( "cooldown", Encode.int tower.cooldown )
        , ( "currentCooldown", Encode.int tower.currentCooldown )
        , ( "targets", Encode.int tower.targets )
        , ( "temporary", Encode.bool tower.temporary )
        , ( "towerType", towerTypeEncoder tower.towerType )
        ]


projectileEncoder : Projectile -> Encode.Value
projectileEncoder projectile =
    Encode.object
        [ ( "enemyId", Encode.int projectile.enemyId )
        , ( "from", positionEncoder projectile.from )
        , ( "ttl", Encode.int projectile.ttl )
        , ( "color", Encode.string projectile.color )
        ]


gameModelEncoder : GameModel -> Encode.Value
gameModelEncoder model =
    Encode.object
        [ ( "board", Encode.array cellEncoder model.board )
        , ( "enemies", Encode.list enemyEncoder model.enemies )
        , ( "enemyIdCount", Encode.int model.enemyIdCount )
        , ( "state", gameStateEncoder model.state )
        , ( "selected", selectedEncoder model.selected )
        , ( "towerIdCount", Encode.int model.towerIdCount )
        , ( "towers", Encode.dict String.fromInt towerEncoder model.towers )
        , ( "projectiles", Encode.list projectileEncoder model.projectiles )
        , ( "hp", Encode.int model.hp )
        , ( "level", Encode.int model.level )
        ]


cellTypeDecoder : Decode.Decoder CellType
cellTypeDecoder =
    let
        getCellObject cellObjectMaybeString =
            cellObjectMaybeString
                |> Maybe.map
                    (\cellObjectString ->
                        if cellObjectString == "Stone" then
                            Stone

                        else if cellObjectString == "Blocked" then
                            Blocked

                        else if String.startsWith "Tower_" cellObjectString then
                            CellTower
                                (cellObjectString
                                    |> String.replace "Tower_" ""
                                    |> String.toInt
                                    |> Maybe.withDefault 0
                                )

                        else
                            NoCellObject
                    )
                |> Maybe.withDefault NoCellObject
    in
    Decode.map2
        (\cellType cellObject ->
            case cellType of
                "Path" ->
                    Path (getCellObject cellObject)

                "Grass" ->
                    Grass (getCellObject cellObject)

                "Start" ->
                    Start

                "Goal" ->
                    Goal

                "Post" ->
                    Post

                _ ->
                    Grass (getCellObject cellObject)
        )
        (Decode.field "type" Decode.string)
        (Decode.field "cellObject" (Decode.nullable Decode.string))


cellDecoder : Decode.Decoder Cell
cellDecoder =
    Decode.map2 Cell
        (Decode.field "cellType" cellTypeDecoder)
        (Decode.field "index" Decode.int)


positionDecoder : Decode.Decoder Position
positionDecoder =
    Decode.succeed Position
        |> Decode.andMap (Decode.field "x" Decode.int)
        |> Decode.andMap (Decode.field "y" Decode.int)


enemyDecoder : Decode.Decoder Enemy
enemyDecoder =
    Decode.succeed Enemy
        |> Decode.andMap (Decode.field "position" positionDecoder)
        |> Decode.andMap (Decode.field "path" (Decode.list positionDecoder))
        |> Decode.andMap (Decode.field "id" Decode.int)
        |> Decode.andMap (Decode.field "hp" Decode.int)
        |> Decode.andMap (Decode.field "maxHp" Decode.int)
        |> Decode.andMap (Decode.field "damage" Decode.int)
        |> Decode.andMap (Decode.field "spawnTime" Decode.int)
        |> Decode.andMap (Decode.field "flying" Decode.bool)
        |> Decode.andMap (Decode.field "boss" Decode.bool)


gameStateDecoder : Decode.Decoder GameState
gameStateDecoder =
    Decode.map
        (\stateString ->
            if stateString == "Level" then
                Level

            else if stateString == "GameOver" then
                GameOver

            else if stateString == "Paused" then
                Paused

            else
                Build
                    (stateString
                        |> String.replace "Build_" ""
                        |> String.toInt
                        |> Maybe.withDefault 0
                    )
        )
        Decode.string


selectedDecoder : Decode.Decoder Selected
selectedDecoder =
    Decode.map2
        (\selectedTypeString selectedId ->
            case selectedTypeString of
                "Tower" ->
                    TowerSelected selectedId

                "Enemy" ->
                    EnemySelected selectedId

                "Stone" ->
                    StoneSelected selectedId

                _ ->
                    NothingSelected
        )
        (Decode.field "selectedType" Decode.string)
        (Decode.field "selectedId" Decode.int)


towerTypeDecoder : Decode.Decoder TowerType
towerTypeDecoder =
    Decode.map towerTypeFromString Decode.string


towerDecoder : Decode.Decoder Tower
towerDecoder =
    Decode.succeed Tower
        |> Decode.andMap (Decode.field "damage" Decode.int)
        |> Decode.andMap (Decode.field "flyingDamage" Decode.float)
        |> Decode.andMap (Decode.field "totalDamage" Decode.int)
        |> Decode.andMap (Decode.field "range" Decode.int)
        |> Decode.andMap (Decode.field "cellIndex" Decode.int)
        |> Decode.andMap (Decode.field "cooldown" Decode.int)
        |> Decode.andMap (Decode.field "currentCooldown" Decode.int)
        |> Decode.andMap (Decode.field "targets" Decode.int)
        |> Decode.andMap (Decode.field "temporary" Decode.bool)
        |> Decode.andMap (Decode.field "towerType" towerTypeDecoder)


projectileDecoder : Decode.Decoder Projectile
projectileDecoder =
    Decode.succeed Projectile
        |> Decode.andMap (Decode.field "enemyId" Decode.int)
        |> Decode.andMap (Decode.field "from" positionDecoder)
        |> Decode.andMap (Decode.field "ttl" Decode.int)
        |> Decode.andMap (Decode.field "color" Decode.string)


gameModelDecoder : Int -> Decode.Decoder GameModel
gameModelDecoder seed =
    Decode.succeed GameModel
        |> Decode.andMap (Decode.field "board" (Decode.array cellDecoder))
        |> Decode.andMap (Decode.field "enemies" (Decode.list enemyDecoder))
        |> Decode.andMap (Decode.field "enemyIdCount" Decode.int)
        |> Decode.andMap (Decode.field "state" gameStateDecoder)
        |> Decode.andMap (Decode.field "selected" selectedDecoder)
        |> Decode.andMap (Decode.field "towerIdCount" Decode.int)
        |> Decode.andMap (Decode.field "towers" (Decode.dict2 Decode.int towerDecoder))
        |> Decode.andMap (Decode.field "projectiles" (Decode.list projectileDecoder))
        |> Decode.andMap (Decode.field "hp" Decode.int)
        |> Decode.andMap (Decode.field "level" Decode.int)
        |> Decode.andMap (Decode.succeed (Random.initialSeed seed))
