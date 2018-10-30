module Debugger exposing (Model, Msg, update, view)

import Bootstrap.Button as Button
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Form.InputGroup as InputGroup
import Bootstrap.Table as Table
import Bootstrap.Utilities.Spacing as Spacing
import Component.CPU exposing (CPU)
import Component.MMU as MMU
import GameBoy exposing (GameBoy)
import Html exposing (Html, abbr, div, h2, li, ol, span, text)
import Html.Attributes exposing (placeholder, style)
import Util


type alias Model =
    { breakpoints : List Int
    , watchpoints : List Int
    , watchRanges : List ( Int, Int )
    , gameBoy : GameBoy
    }


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


view : Model -> Html msg
view model =
    div []
        [ h2 [] [ text "Debugger" ]
        , Button.button [ Button.outlineSecondary ] [ text "Trace" ]
        , InputGroup.config
            (InputGroup.text [ Input.placeholder "0x0000" ])
            |> InputGroup.successors
                [ InputGroup.button [ Button.secondary ] [ text "Run" ] ]
            |> InputGroup.view
        , viewRegisters model.gameBoy.cpu
        , viewMisc model.gameBoy
        , viewMemoryDebugger model.gameBoy
        ]


viewRegisters : CPU -> Html msg
viewRegisters cpu =
    Table.table
        { options = [ Table.small ]
        , thead =
            Table.simpleThead
                [ Table.th [ Table.cellAttr (style "width" "16.6666666667%") ] [ text "AF" ]
                , Table.th [ Table.cellAttr (style "width" "16.6666666667%") ] [ text "BC" ]
                , Table.th [ Table.cellAttr (style "width" "16.6666666667%") ] [ text "DE" ]
                , Table.th [ Table.cellAttr (style "width" "16.6666666667%") ] [ text "HL" ]
                , Table.th [ Table.cellAttr (style "width" "16.6666666667%") ] [ text "SP" ]
                , Table.th [ Table.cellAttr (style "width" "16.6666666667%") ] [ text "PC" ]
                ]
        , tbody =
            Table.tbody []
                [ Table.tr []
                    [ Table.td [] [ cpu.af |> Util.word16ToString |> monospacedText ]
                    , Table.td [] [ cpu.bc |> Util.word16ToString |> monospacedText ]
                    , Table.td [] [ cpu.de |> Util.word16ToString |> monospacedText ]
                    , Table.td [] [ cpu.hl |> Util.word16ToString |> monospacedText ]
                    , Table.td [] [ cpu.sp |> Util.word16ToString |> monospacedText ]
                    , Table.td [] [ cpu.pc |> Util.word16ToString |> monospacedText ]
                    ]
                ]
        }


viewMisc : GameBoy -> Html msg
viewMisc gameBoy =
    Table.table
        { options = [ Table.small ]
        , thead =
            Table.simpleThead
                [ Table.th [ Table.cellAttr (style "width" "16.6666666667%") ] [ text "LCDC" ]
                , Table.th [ Table.cellAttr (style "width" "16.6666666667%") ] [ text "STAT" ]
                , Table.th [ Table.cellAttr (style "width" "16.6666666667%") ] [ text "LY" ]
                , Table.th [ Table.cellAttr (style "width" "16.6666666667%") ] [ text "IE" ]
                , Table.th [ Table.cellAttr (style "width" "16.6666666667%") ] [ text "IF" ]
                , Table.th [ Table.cellAttr (style "width" "16.6666666667%") ] [ text "IMA" ]
                ]
        , tbody =
            Table.tbody []
                [ Table.tr []
                    [ Table.td [] [ gameBoy.ppu.lcdc |> Util.word8ToString |> monospacedText ]
                    , Table.td [] [ gameBoy.ppu.lcdStatus |> Util.word8ToString |> monospacedText ]
                    , Table.td [] [ gameBoy.ppu.line |> Util.word8ToString |> monospacedText ]
                    , Table.td [] [ gameBoy.cpu.interruptEnable |> Util.word8ToString |> monospacedText ]
                    , Table.td [] [ gameBoy.cpu.interruptFlag |> Util.word8ToString |> monospacedText ]
                    , Table.td [] [ gameBoy.cpu.interruptMasterEnable |> Util.boolToString |> monospacedText ]
                    ]
                ]
        }


viewMemoryDebugger : GameBoy -> Html msg
viewMemoryDebugger gameBoy =
    div []
        [ Form.formInline []
            [ Input.text [ Input.attrs [ placeholder "0x0000", Spacing.mr2 ] ], Input.text [ Input.attrs [ placeholder "0xFFFF" ] ] ]
        , viewMemory 0xFF00 0xFF gameBoy
        ]


viewMemory : Int -> Int -> GameBoy -> Html msg
viewMemory address length gameBoy =
    Table.table
        { options = [ Table.small ]
        , thead =
            Table.simpleThead []
        , tbody =
            List.range address (address + length)
                |> Util.chunkList 16
                |> List.map
                    (\addresses ->
                        let
                            values =
                                addresses
                                    |> List.map (MMU.readWord8 gameBoy)
                                    |> List.map (\value -> Table.td [] [ value |> Util.word8ToString |> monospacedText ])
                        in
                        Table.tr []
                            (Table.th []
                                [ addresses
                                    |> List.head
                                    |> Maybe.map Util.word16ToString
                                    |> Maybe.withDefault "0x????"
                                    |> monospacedText
                                ]
                                :: values
                            )
                    )
                |> Table.tbody []
        }


monospacedText : String -> Html msg
monospacedText string =
    span [ style "font-family" "Courier New" ] [ text string ]
