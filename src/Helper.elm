module Helper exposing (actionButtonsPosition, intToPxString)

import Constants exposing (boardWidth)
import Html
import Html.Attributes
import Types exposing (CellIndex)


intToPxString : Int -> String
intToPxString value =
    String.fromInt value ++ "px"


actionButtonsPosition : CellIndex -> Html.Attribute msg
actionButtonsPosition cellIndex =
    let
        col =
            modBy boardWidth cellIndex

        row =
            cellIndex // boardWidth
    in
    if col >= boardWidth - 3 then
        Html.Attributes.style "right" "35px"

    else if row == 0 then
        Html.Attributes.style "bottom" "-20px"

    else
        Html.Attributes.style "top" "-20px"
