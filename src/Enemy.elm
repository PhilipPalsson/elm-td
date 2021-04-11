module Enemy exposing (Enemy, EnemyId, codec)

import Position as Position exposing (Position)
import Serialize as S exposing (Codec)


type alias EnemyId =
    Int


type alias Enemy =
    { position : Position
    , path : List Position
    , id : Int
    , hp : Int
    , maxHp : Int
    , damage : Int
    , spawnTime : Int
    , evasion : Int
    , flying : Bool
    , boss : Bool
    , slowEffects : List SlowEffectData
    , baseSpeed : Int
    , magicImmune : Bool
    , dieDelay : Int
    }


type alias SlowEffectData =
    { duration : Int, amount : Int }


slowEffectCodec : Codec e SlowEffectData
slowEffectCodec =
    S.record SlowEffectData
        |> S.field .duration S.int
        |> S.field .amount S.int
        |> S.finishRecord


codec : Codec e Enemy
codec =
    S.record Enemy
        |> S.field .position Position.codec
        |> S.field .path (S.list Position.codec)
        |> S.field .id S.int
        |> S.field .hp S.int
        |> S.field .maxHp S.int
        |> S.field .damage S.int
        |> S.field .spawnTime S.int
        |> S.field .evasion S.int
        |> S.field .flying S.bool
        |> S.field .boss S.bool
        |> S.field .slowEffects (S.list slowEffectCodec)
        |> S.field .baseSpeed S.int
        |> S.field .magicImmune S.bool
        |> S.field .dieDelay S.int
        |> S.finishRecord
