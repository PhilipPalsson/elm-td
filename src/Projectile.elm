module Projectile exposing (Projectile, codec)

import Enemy exposing (EnemyId)
import Position as Position exposing (Position)
import Serialize as S exposing (Codec)


type alias Projectile =
    { enemyId : EnemyId
    , from : Position
    , timeToLive : Int
    , color : String
    , miss : Bool
    }


codec : Codec e Projectile
codec =
    S.record Projectile
        |> S.field .enemyId S.int
        |> S.field .from Position.codec
        |> S.field .timeToLive S.int
        |> S.field .color S.string
        |> S.field .miss S.bool
        |> S.finishRecord
