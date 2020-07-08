module Helper exposing (actionButtonsPosition, imageAttributes, intToPxString)

import Constants exposing (boardWidth)
import Html
import Html.Attributes exposing (style)
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


imageAttributes : String -> String -> List (Html.Attribute msg)
imageAttributes image size =
    [ style "background-image" ("url(%PUBLIC_URL%/images/" ++ image ++ ")")
    , style "background-position-x" "center"
    , style "background-position-y" "center"
    , style "background-size" size
    , style "background-repeat-x" "no-repeat"
    , style "background-repeat-y" "no-repeat"
    ]
