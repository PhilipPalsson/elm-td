module Cell exposing (Cell, CellIndex, CellObject(..), CellType(..), cellCodec)

import Serialize as S exposing (Codec)
import Tower exposing (TowerId)


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


type alias CellIndex =
    Int


cellObjectCodec : Codec e CellObject
cellObjectCodec =
    S.customType
        (\a b c d value ->
            case value of
                CellTower data ->
                    a data

                Stone ->
                    b

                Blocked ->
                    c

                NoCellObject ->
                    d
        )
        |> S.variant1 CellTower S.int
        |> S.variant0 Stone
        |> S.variant0 Blocked
        |> S.variant0 NoCellObject
        |> S.finishCustomType


cellTypeCodec : Codec e CellType
cellTypeCodec =
    S.customType
        (\a b c d e value ->
            case value of
                Path data ->
                    a data

                Grass data ->
                    b data

                Start ->
                    c

                Goal ->
                    d

                Post ->
                    e
        )
        |> S.variant1 Path cellObjectCodec
        |> S.variant1 Grass cellObjectCodec
        |> S.variant0 Start
        |> S.variant0 Goal
        |> S.variant0 Post
        |> S.finishCustomType


cellCodec : Codec e Cell
cellCodec =
    S.record Cell
        |> S.field .cellType cellTypeCodec
        |> S.field .index S.int
        |> S.finishRecord
