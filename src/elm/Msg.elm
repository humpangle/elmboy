module Msg exposing (Msg(..))

import Array exposing (Array)
import Component.Joypad exposing (GameBoyButton)
import Debugger


type Msg
    = FileSelected
    | FileDataReceived (Array Int)
    | AnimationFrameDelta Float
    | ButtonDown (Maybe GameBoyButton)
    | ButtonUp (Maybe GameBoyButton)
    | Reset
    | Pause
    | Resume
    | CloseErrorModal
    | DebuggerMsg Debugger.Msg
