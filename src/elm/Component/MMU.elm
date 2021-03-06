module Component.MMU exposing
    ( readWord16
    , readWord8
    , readWord8Chunk
    , writeWord16
    , writeWord8
    , writeWord8Chunk
    )

import Bitwise
import Component.CPU as CPU exposing (CPU)
import Component.Cartridge as Cartridge exposing (Cartridge)
import Component.Joypad as Joypad exposing (Joypad)
import Component.PPU as PPU
import Component.PPU.Types exposing (PPU)
import Component.RAM as RAM exposing (RAM)
import Component.Timer as Timer exposing (Timer)
import GameBoy exposing (GameBoy)
import Types exposing (MemoryAddress)


readWord8 : GameBoy -> MemoryAddress -> Int
readWord8 gameBoy address =
    {-
       This could be solved more elegantly, but as memory access is happening every cycle, we have to implement this in the most
       performant way with as little as possible overhead.
    -}
    if address <= 0x0100 && not gameBoy.bootRomDisabled then
        -- Practically, those values will never be read anymore as the bootrom support was removed during early development to avoid
        -- copyright and trademark issues. But if they are, we just treat the whole boot ROM as a series of NOPs.
        0x00

    else if address <= 0x7FFF then
        -- Cartridge ROM
        Cartridge.readWord8 gameBoy.cartridge address

    else if address >= 0x8000 && address <= 0x9FFF then
        -- VRAM
        PPU.readVRAM gameBoy.ppu (address - 0x8000)

    else if address >= 0xA000 && address <= 0xBFFF then
        -- Cartridge RAM
        Cartridge.readWord8 gameBoy.cartridge address

    else if address >= 0xC000 && address <= 0xCFFF then
        -- Work RAM Bank 0
        RAM.readWord8 gameBoy.workRamBank0 (address - 0xC000)

    else if address >= 0xD000 && address <= 0xDFFF then
        -- Work RAM Bank 1
        RAM.readWord8 gameBoy.workRamBank1 (address - 0xD000)

    else if address >= 0xE000 && address <= 0xEFFF then
        -- Work RAM Bank 0 (Mirror)
        RAM.readWord8 gameBoy.workRamBank0 (address - 0xE000)

    else if address >= 0xF000 && address <= 0xFDFF then
        -- Work RAM Bank 1 (Partial Mirror)
        RAM.readWord8 gameBoy.workRamBank1 (address - 0xF000)

    else if address >= 0xFE00 && address <= 0xFE9F then
        -- OAM
        PPU.readOamRam gameBoy.ppu (address - 0xFE00)

    else if address >= 0xFF80 && address <= 0xFFFE then
        -- HRAM
        RAM.readWord8 gameBoy.hram (address - 0xFF80)

    else if address == 0xFF00 then
        Joypad.readRegister gameBoy.joypad

    else if address == 0xFF04 then
        Timer.readDivider gameBoy.timer

    else if address == 0xFF05 then
        Timer.readTima gameBoy.timer

    else if address == 0xFF06 then
        Timer.readTma gameBoy.timer

    else if address == 0xFF07 then
        Timer.readTac gameBoy.timer

    else if address == 0xFF40 then
        PPU.readLCDC gameBoy.ppu

    else if address == 0xFF41 then
        PPU.readLCDStatus gameBoy.ppu

    else if address == 0xFF42 then
        PPU.readScrollY gameBoy.ppu

    else if address == 0xFF43 then
        PPU.readScrollX gameBoy.ppu

    else if address == 0xFF44 then
        PPU.readLY gameBoy.ppu

    else if address == 0xFF45 then
        PPU.readLYC gameBoy.ppu

    else if address == 0xFF47 then
        PPU.readBackgroundPalette gameBoy.ppu

    else if address == 0xFF48 then
        PPU.readObjectPalette0 gameBoy.ppu

    else if address == 0xFF49 then
        PPU.readObjectPalette1 gameBoy.ppu

    else if address == 0xFF4A then
        PPU.readWindowY gameBoy.ppu

    else if address == 0xFF4B then
        PPU.readWindowX gameBoy.ppu

    else if address == 0xFF50 then
        if gameBoy.bootRomDisabled then
            0x01

        else
            0x00

    else if address == 0xFF0F then
        gameBoy.cpu.interruptFlag

    else if address == 0xFFFF then
        gameBoy.cpu.interruptEnable

    else
        0xFF


writeWord8 : MemoryAddress -> Int -> GameBoy -> GameBoy
writeWord8 address value ({ cpu } as gameBoy) =
    let
        sanitizedValue =
            Bitwise.and 0xFF value
    in
    if address <= 0x7FFF then
        -- Cartridge ROM, will be intercepted by a possible memory bank controller
        GameBoy.setCartridge (Cartridge.writeWord8 address value gameBoy.cartridge) gameBoy

    else if address >= 0x8000 && address <= 0x9FFF then
        -- VRAM
        GameBoy.setPPU (PPU.writeVRAM (address - 0x8000) sanitizedValue gameBoy.ppu) gameBoy

    else if address >= 0xA000 && address <= 0xBFFF then
        -- Cartridge RAM
        GameBoy.setCartridge (Cartridge.writeWord8 address value gameBoy.cartridge) gameBoy

    else if address >= 0xC000 && address <= 0xCFFF then
        -- Work RAM Bank 0
        GameBoy.setWorkRamBank0 (RAM.writeWord8 (address - 0xC000) sanitizedValue gameBoy.workRamBank0) gameBoy

    else if address >= 0xD000 && address <= 0xDFFF then
        -- Work RAM Bank 1
        GameBoy.setWorkRamBank1 (RAM.writeWord8 (address - 0xD000) sanitizedValue gameBoy.workRamBank1) gameBoy

    else if address >= 0xE000 && address <= 0xEFFF then
        -- Work RAM Bank 0 (Mirror)
        GameBoy.setWorkRamBank0 (RAM.writeWord8 (address - 0xE000) sanitizedValue gameBoy.workRamBank0) gameBoy

    else if address >= 0xF000 && address <= 0xFDFF then
        -- Work RAM Bank 1 (Partial Mirror)
        GameBoy.setWorkRamBank1 (RAM.writeWord8 (address - 0xF000) sanitizedValue gameBoy.workRamBank1) gameBoy

    else if address >= 0xFE00 && address <= 0xFE9F then
        -- OAM
        GameBoy.setPPU (PPU.writeOAMRam (address - 0xFE00) sanitizedValue gameBoy.ppu) gameBoy

    else if address >= 0xFF80 && address <= 0xFFFE then
        -- HRAM
        GameBoy.setHRAM (RAM.writeWord8 (address - 0xFF80) sanitizedValue gameBoy.hram) gameBoy

    else if address == 0xFF00 then
        GameBoy.setJoypad (Joypad.writeRegister sanitizedValue gameBoy.joypad) gameBoy

    else if address == 0xFF04 then
        GameBoy.setTimer (Timer.resetDivider gameBoy.timer) gameBoy

    else if address == 0xFF05 then
        GameBoy.setTimer (Timer.writeTima sanitizedValue gameBoy.timer) gameBoy

    else if address == 0xFF06 then
        GameBoy.setTimer (Timer.writeTma sanitizedValue gameBoy.timer) gameBoy

    else if address == 0xFF07 then
        GameBoy.setTimer (Timer.writeTac sanitizedValue gameBoy.timer) gameBoy

    else if address == 0xFF40 then
        GameBoy.setPPU (PPU.writeLCDC sanitizedValue gameBoy.ppu) gameBoy

    else if address == 0xFF41 then
        GameBoy.setPPU (PPU.writeLCDStatus sanitizedValue gameBoy.ppu) gameBoy

    else if address == 0xFF42 then
        GameBoy.setPPU (PPU.writeScrollY sanitizedValue gameBoy.ppu) gameBoy

    else if address == 0xFF43 then
        GameBoy.setPPU (PPU.writeScrollX sanitizedValue gameBoy.ppu) gameBoy

    else if address == 0xFF44 then
        GameBoy.setPPU (PPU.writeLY sanitizedValue gameBoy.ppu) gameBoy

    else if address == 0xFF45 then
        GameBoy.setPPU (PPU.writeLYC sanitizedValue gameBoy.ppu) gameBoy

    else if address == 0xFF46 then
        oamDMATransfer sanitizedValue gameBoy

    else if address == 0xFF47 then
        GameBoy.setPPU (PPU.writeBackgroundPalette sanitizedValue gameBoy.ppu) gameBoy

    else if address == 0xFF48 then
        GameBoy.setPPU (PPU.writeObjectPalette0 sanitizedValue gameBoy.ppu) gameBoy

    else if address == 0xFF49 then
        GameBoy.setPPU (PPU.writeObjectPalette1 sanitizedValue gameBoy.ppu) gameBoy

    else if address == 0xFF4A then
        GameBoy.setPPU (PPU.writeWindowY sanitizedValue gameBoy.ppu) gameBoy

    else if address == 0xFF4B then
        GameBoy.setPPU (PPU.writeWindowX sanitizedValue gameBoy.ppu) gameBoy

    else if address == 0xFF50 then
        { gameBoy | bootRomDisabled = True }

    else if address == 0xFF0F then
        GameBoy.setCPU (CPU.setInterruptFlag sanitizedValue gameBoy.cpu) gameBoy

    else if address == 0xFFFF then
        GameBoy.setCPU (CPU.setInterruptEnable sanitizedValue gameBoy.cpu) gameBoy

    else
        gameBoy


readWord16 : GameBoy -> MemoryAddress -> Int
readWord16 gameBoy address =
    let
        highByte =
            readWord8 gameBoy (address + 1)

        lowByte =
            readWord8 gameBoy address
    in
    Bitwise.or (Bitwise.shiftLeftBy 8 highByte) lowByte


writeWord16 : MemoryAddress -> Int -> GameBoy -> GameBoy
writeWord16 address value gameBoy =
    let
        highByte =
            Bitwise.and 0xFF00 value |> Bitwise.shiftRightZfBy 8

        lowByte =
            Bitwise.and 0xFF value
    in
    gameBoy
        |> writeWord8 address lowByte
        |> writeWord8 (address + 1) highByte


readWord8Chunk : GameBoy -> MemoryAddress -> Int -> List Int
readWord8Chunk gameBoy startAddress length =
    List.repeat length startAddress
        |> List.indexedMap (+)
        |> List.map (readWord8 gameBoy)


writeWord8Chunk : GameBoy -> MemoryAddress -> List Int -> GameBoy
writeWord8Chunk gameBoy startAddress bytes =
    case bytes of
        [] ->
            gameBoy

        head :: tail ->
            let
                updatedGameBoy =
                    writeWord8 startAddress head gameBoy
            in
            writeWord8Chunk updatedGameBoy (startAddress + 1) tail



-- Helpers


oamDMATransfer : Int -> GameBoy -> GameBoy
oamDMATransfer byte gameBoy =
    let
        chunk =
            readWord8Chunk gameBoy (Bitwise.shiftLeftBy 8 byte) (40 * 4)
    in
    GameBoy.setPPU (PPU.replaceOAMRam chunk gameBoy.ppu) gameBoy
