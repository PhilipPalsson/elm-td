port module Ports exposing (deleteSaveState, saveState)

import Json.Encode as Encode


port saveState : Encode.Value -> Cmd msg


port deleteSaveState : () -> Cmd msg
