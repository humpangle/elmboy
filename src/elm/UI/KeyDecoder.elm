module UI.KeyDecoder exposing (KeyboardShortcut(..), decodeGameBoyButton, decodeKeyboardShortcut)

import Component.Joypad exposing (GameBoyButton(..))
import Json.Decode as Decode


type KeyboardShortcut
    = EnterDebugMode


decodeGameBoyButton : Decode.Decoder GameBoyButton
decodeGameBoyButton =
    Decode.field "key" Decode.string
        |> Decode.andThen (mapGameBoyButton >> Maybe.map Decode.succeed >> Maybe.withDefault (Decode.fail "Not a GameBoyButton!"))


decodeKeyboardShortcut : Decode.Decoder KeyboardShortcut
decodeKeyboardShortcut =
    Decode.map2
        (\key ctrl ->
            if ctrl && key == "d" then
                Decode.succeed EnterDebugMode

            else
                Decode.fail "Not a KeyboardShortcut!"
        )
        (Decode.field "key" Decode.string)
        (Decode.field "ctrlKey" Decode.bool)
        |> Decode.andThen identity


mapKeyboardShortcut : String -> Maybe KeyboardShortcut
mapKeyboardShortcut s =
    Nothing


mapGameBoyButton : String -> Maybe GameBoyButton
mapGameBoyButton string =
    case string of
        "ArrowLeft" ->
            Just Left

        "ArrowRight" ->
            Just Right

        "ArrowUp" ->
            Just Up

        "ArrowDown" ->
            Just Down

        "s" ->
            Just A

        "a" ->
            Just B

        "Enter" ->
            Just Start

        "Shift" ->
            Just Select

        _ ->
            Nothing
