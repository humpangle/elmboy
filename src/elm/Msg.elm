module Msg exposing (Msg(..))

import Array exposing (Array)
import Component.Joypad exposing (GameBoyButton)
import Debugger
import UI.KeyDecoder exposing (KeyboardShortcut)


type Msg
    = FileSelected
    | FileDataReceived (Array Int)
    | AnimationFrameDelta Float
    | ButtonDown GameBoyButton
    | ButtonUp GameBoyButton
    | Keyboard KeyboardShortcut
    | Reset
    | Pause
    | Resume
    | CloseErrorModal
    | DebuggerMsg Debugger.Msg
