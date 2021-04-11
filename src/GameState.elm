module GameState exposing (GameState(..), codec)

import Serialize as S exposing (Codec)


type GameState
    = Level
    | Build Int
    | GameOver
    | GameCompleted
    | Paused


codec : Codec e GameState
codec =
    S.customType
        (\a b c d e value ->
            case value of
                Level ->
                    a

                Build data ->
                    b data

                GameOver ->
                    c

                Paused ->
                    d

                GameCompleted ->
                    e
        )
        |> S.variant0 Level
        |> S.variant1 Build S.int
        |> S.variant0 GameOver
        |> S.variant0 Paused
        |> S.variant0 GameCompleted
        |> S.finishCustomType
