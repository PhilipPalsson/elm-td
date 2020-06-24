port module Ports exposing (saveState)

import Json.Encode as Encode


port saveState : Encode.Value -> Cmd msg
