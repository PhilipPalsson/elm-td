module Position exposing (Position, codec)

import Serialize as S exposing (Codec)


type alias Position =
    { x : Int, y : Int }


codec : Codec e Position
codec =
    S.record Position
        |> S.field .x S.int
        |> S.field .y S.int
        |> S.finishRecord
