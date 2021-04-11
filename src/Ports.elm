port module Ports exposing (deleteSaveState, saveState)


port saveState : String -> Cmd msg


port deleteSaveState : () -> Cmd msg
