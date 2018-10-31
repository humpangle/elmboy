module Debugger exposing (Model, Msg, init, subscriptions, update, view)

import Array exposing (Array)
import Bootstrap.Button as Button
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Form.InputGroup as InputGroup
import Bootstrap.Grid as Grid
import Bootstrap.Table as Table
import Bootstrap.Utilities.Spacing as Spacing
import Component.CPU exposing (CPU)
import Component.Cartridge as Cartridge
import Component.MMU as MMU
import GameBoy exposing (GameBoy)
import Html exposing (Html, abbr, canvas, div, h1, h2, input, li, ol, small, span, text)
import Html.Attributes exposing (class, height, id, placeholder, style, type_, width)
import Html.Events exposing (onInput)
import Ports
import Util


type alias Model =
    { breakpoints : List Int
    , watchpoints : List Int
    , watchRanges : List ( Int, Int )
    , gameBoy : Maybe GameBoy
    , autoRun : Bool
    }


type Msg
    = RomFileSelected
    | RomFileLoaded (Array Int)


init : ( Model, Cmd Msg )
init =
    ( { breakpoints = [], watchpoints = [], watchRanges = [], gameBoy = Nothing, autoRun = False }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RomFileSelected ->
            ( model, Ports.requestFileData "romFileInput" )

        RomFileLoaded rom ->
            ( { model | gameBoy = Cartridge.fromBytes rom |> Maybe.map GameBoy.init }, Cmd.none )


view : Model -> Html Msg
view model =
    let
        content =
            case model.gameBoy of
                Just gameBoy ->
                    div []
                        [ canvas [ width 160, height 144, id "screen" ] []
                        , Button.button [ Button.outlineSecondary ] [ text "Trace" ]
                        , InputGroup.config
                            (InputGroup.text [ Input.placeholder "0x0000" ])
                            |> InputGroup.successors
                                [ InputGroup.button [ Button.secondary ] [ text "Run" ] ]
                            |> InputGroup.view
                        , viewRegisters gameBoy.cpu
                        , viewMisc gameBoy
                        , viewMemoryDebugger gameBoy
                        ]

                Nothing ->
                    div [] [ input [ type_ "file", id "romFileInput", onInput (\_ -> RomFileSelected) ] [ text "Select a ROM" ] ]
    in
    Grid.containerFluid [] [ h1 [] [ text "Elmboy", small [ class "text-muted" ] [ text " Debugger" ] ], content ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Ports.fileData RomFileLoaded


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
