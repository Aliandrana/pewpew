.INCLUDE "header.asm"
.INCLUDE "init.asm"
.INCLUDE "registers.asm"



; Memory layout:
; 0000-000F: scratch space for functions.
; 0010-0011: controller state of joypad #1.
; 0012-0013: controller state of joypad #2.
; 0014-0016: 24-bit counter of vblanks.
; 0017-0019: RGB color values to use for background color, from [0-31].
; 001A-001B: 16-bit pointer to next random byte.
; [gap]
; 0020-0021: (x, y) coordinates of player.
; 0022: shot cooldown timer.
; 0023: next-shot state.
; [gap]
; 0030-008F: {sprite, x, y, x-velocity, y-velocity, unused} per shot.
;            If sprite is 0, the shot is disabled.
; [gap]
; Sprite table buffers -- copied each frame to OAM during VBlank, using DMA.
; 1000-11FF: table 1 (4 bytes each: x/y coord, tile #, flip/priority/palette)
; 1200-121F: table 2 (2 bits each: high x-coord bit, size)
; 1220-12A0: scratch table. One byte per sprite for high x-coord & size.
.define joy1 $10
.define joy2 $12
.define vBlankCounter $14
.define backgroundRed $17
.define backgroundGreen $18
.define backgroundBlue $19
.define randomBytePtr $1A
.define playerX $20
.define playerY $21
.define shotCooldown $22
.define nextShotState $23
.define shotArray $30
.define shotArrayLength 16
.define shotSize 6

.define numSprites 128
.define spriteTableStart $1000
.define spriteTable1Size $200
.define spriteTable2Start $1200
.define spriteTableSize $220
.define spriteTableScratchStart $1220



; Sets A to 8-bit (& enables 8-bit "B" register).
.MACRO SetA8Bit
sep #%00100000  ; 8-bit A/B.
.ENDM



; Sets A to 16-bit.
.MACRO SetA16Bit
rep #%00100000  ; 16-bit A.
.ENDM



; Sets X/Y to 16-bit.
.MACRO SetXY16Bit
rep #%00010000  ; 16-bit X/Y.
.ENDM



; Stores result to A.
; Assumes 16-bit X & 8-bit A.
; Modifies X.
; Updates randomBytePtr.
.MACRO GetRandomByte
    ldx randomBytePtr
    lda $028000, X  ; $028000: beginning of ROM bank 2.
    inx
    cpx #$8000  ; This is the size of the entire ROM bank.
    bne +++
    ldx #0
+++
    stx randomBytePtr
.ENDM



.BANK 0 SLOT 0
.ORG 0
.SECTION "MainCode"



Start:
    InitializeSNES

    ; By default we assume 16-bit X/Y and 8-bit A.
    ; If any code wants to change this, it's expected to do so itself,
    ; and to change them back to the defaults before returning.
    SetXY16Bit
    SetA8Bit

    jsr LoadPaletteAndTileData
    jsr InitializeSpriteTables
    jsr InitializeWorld

    ; Set screen mode: 16x16 tiles for backgrounds, mode 1.
    lda #%11000001
    sta BGMODE

    ; Set sprite size to 16x16 (small) and 32x32 (large).
    lda #%01100000
    sta OAMSIZE

    ; Main screen: enable sprites & BG3.
    lda #%00010100
    sta MSENABLE

    ; Turn on the screen.
    ; Format: x000bbbb
    ; x: 0 = screen on, 1 = screen off, bbbb: Brightness ($0-$F)
    lda #%00001111
    sta INIDISP

    jmp MainLoop



LoadPaletteAndTileData:
    ; For more details on DMA, see:
    ; http://wiki.superfamicom.org/snes/show/Grog%27s+Guide+to+DMA+and+HDMA+on+the+SNES
    ; http://wiki.superfamicom.org/snes/show/Making+a+Small+Game+-+Tic-Tac-Toe
    ;
    ; A lot of the graphics-related registers are explained in Qwertie's doc:
    ; http://emu-docs.org/Super%20NES/General/snesdoc.html
    ; ... but be careful, because there are some errors in this doc.
    ;
    ; bazz's tutorial (available from http://wiki.superfamicom.org/snes/) is
    ; quite helpful with palette / sprites / DMA, especially starting at
    ; http://wiki.superfamicom.org/snes/show/Working+with+VRAM+-+Loading+the+Palette

    ; Initialize the palette memory in a loop.
    ; We could also do this with a DMA transfer (like we do with the tile data
    ; below), but it seems overkill for just a few bytes. :)
    ; TODO(mcmillen): do it with a DMA transfer.

    ; First, sprite palette data:
    ldx #0
    lda #128  ; Palette entries for sprites start at 128.
    sta CGADDR
-
    lda.l SpritePalette, X
    sta CGDATA
    inx
    cpx #32  ; 32 bytes of palette data.
    bne -

    ; Now, BG3 palette data.
    ; Palette entries for BG3 start at 0.
    ldx #0
    lda #0
    sta CGADDR
-
    lda.l TilePalette, X
    sta CGDATA
    inx
    cpx #8  ; 8 bytes of palette data.
    bne -

    ; TODO(mcmillen): make the "DMA stuff into VRAM" a macro or function.
    ; Set VMADDR to where we want the DMA to start.  We'll store sprite data
    ; at the beginning of VRAM.
    ldx #$0000
    stx VMADDR
    ; DMA 0 source address & bank.
    ldx #SpriteData
    stx DMA0SRC
    lda #:SpriteData
    sta DMA0SRCBANK
    ; DMA 0 transfer size.  Equal to the size of sprites32.pic.
    ldx #2048
    stx DMA0SIZE
    ; DMA 0 control register.
    ; Transfer type 001 = 2 addresses, LH.
    lda #%00000001
    sta DMA0CTRL
    ; DMA 0 destination.
    lda #$18  ; The upper byte is assumed to be $21, so this is $2118 & $2119.
    sta DMA0DST
    ; Enable DMA channel 0.
    lda #%00000001
    sta DMAENABLE

    ; Store background tile data at byte $2000 of VRAM.
    ; (VMADDR is a word address, so multiply by 2 to get the byte address.)
    ldx #$1000
    stx VMADDR
    ; DMA 0 source address & bank.
    ldx #TileData
    stx DMA0SRC
    lda #:TileData
    sta DMA0SRCBANK
    ; DMA 0 transfer size.  Equal to the size of tiles.pic.
    ldx #512
    stx DMA0SIZE
    ; DMA 0 control register.
    ; Transfer type 001 = 2 addresses, LH.
    lda #%00000001
    sta DMA0CTRL
    ; DMA 0 destination.
    lda #$18  ; The upper byte is assumed to be $21, so this is $2118 & $2119.
    sta DMA0DST
    ; Enable DMA channel 0.
    lda #%00000001
    sta DMAENABLE

    ; Tell the system that the BG3 tilemap starts at $4000.
    lda #%00100000
    sta BG3TILEMAP
    ; ... and that the background tile data for BG3 starts at $2000.
    lda #%00000001
    sta BG34NBA

    ; Set up the BG3 tilemap.
    ; VRAM write mode: increments the address every time we write a word.
    lda #%10000000
    sta VMAIN
    ; Set word address for accessing VRAM.
    ldx #$2000  ; BG 3 tilemap starts here. (Byte address $4000.)
    stx VMADDR
    ; Now write entries into the tile map.
    ldy #0
-
    GetRandomByte
    sta $00
    ldx #$0000  ; This is a blank tile.
    ; 1 in 8 chance that we choose a non-blank tile.
    bit #%00000111
    bne +
    ldx #$0002
    bit #%10000000
    bne +
    ldx #$8002  ; Flip vertically.
+
    stx VMDATA
    iny
    ; The tile map is 32x32 (1024 entries).
    cpy #1024
    bne -

    rts



InitializeSpriteTables:
    ; This page is a good reference on SNES sprite formats:
    ; http://wiki.superfamicom.org/snes/show/SNES+Sprites
    ; It uses the same approach we're using, in which we keep a buffer of the
    ; sprite tables in RAM, and DMA the sprite tables to the system's OAM
    ; during VBlank.
    SetA16Bit

    ldx #$0000
    ; Fill sprite table 1.  4 bytes per sprite, laid out as follows:
    ; Byte 1:    xxxxxxxx    x: X coordinate
    ; Byte 2:    yyyyyyyy    y: Y coordinate
    ; Byte 3:    cccccccc    c: Starting tile #
    ; Byte 4:    vhoopppc    v: vertical flip h: horizontal flip  o: priority bits
    ;                        p: palette #
    lda #$01
-
    sta spriteTableStart, X
    .rept 4
        inx
    .endr
    cpx #spriteTable1Size
    bne -

    ; Fill sprite table 2.  2 bits per sprite, like so:
    ; bits 0,2,4,6 - High bit of the sprite's x-coordinate.
    ; bits 1,3,5,7 - Toggle Sprite size: 0 - small size   1 - large size
    ; Setting all the high bits keeps the sprites offscreen.
    lda #$FFFF
-
    sta spriteTableStart, X
    inx
    inx
    cpx #spriteTableSize
    bne -

    SetA8Bit
    rts



InitializeWorld:
    ; Start the background color as a dark blue.
    lda #4
    sta backgroundBlue

    ; Player's initial starting location.
    lda #(256 / 4)
    sta playerX
    lda #((224 - 32) / 2)
    sta playerY

    rts



MainLoop:
    lda #%10000001  ; Enable NMI interrupt & auto joypad read.
    sta NMITIMEN
    wai  ; Wait for interrupt.
    lda #%00000001  ; Disable NMI interrupt while processing.
    sta NMITIMEN

    jsr JoypadRead
    jsr JoypadHandler
    jsr UpdateWorld
    jsr UpdateSprites
    jsr FillSecondarySpriteTable
    jsr SetBackgroundColor
    jmp MainLoop



JoypadRead:
    ; Load joypad registers into RAM for easy inspection & manipulation.
-
    lda HVBJOY
    bit #$01  ; If auto-joypad read is happening, loop.
    bne -
    ldx JOY1L
    stx joy1
    ldx JOY2L
    stx joy2
    rts



JoypadHandler:
JoypadUp:
    lda joy1 + 1
    bit #$08  ; Up
    beq JoypadDown  ; Button not pressed.
    lda playerY
    cmp #0
    beq JoypadDown  ; Value saturated.
    dec playerY
    dec playerY

JoypadDown:
    lda joy1 + 1
    bit #$04  ; Down
    beq JoypadLeft  ; Button not pressed.
    lda playerY
    cmp #(224 - 32)
    beq JoypadLeft  ; Value saturated.
    inc playerY
    inc playerY

JoypadLeft:
    lda joy1 + 1
    bit #$02  ; Left
    beq JoypadRight  ; Button not pressed.
    lda playerX
    cmp #0
    beq JoypadRight  ; Value saturated.
    dec playerX
    dec playerX

JoypadRight:
    lda joy1 + 1
    bit #$01  ; Right
    beq JoypadStart  ; Button not pressed.
    lda playerX
    cmp #(256 - 32)
    beq JoypadStart  ; Value saturated.
    inc playerX
    inc playerX

JoypadStart:
    lda joy1 + 1
    bit #$10  ; Start
    beq JoypadSelect  ; Button not pressed.
    lda backgroundRed
    cmp #31
    beq JoypadSelect  ; Value saturated.
    inc backgroundRed

JoypadSelect:
    lda joy1 + 1
    bit #$20  ; Select
    beq JoypadY  ; Button not pressed.
    lda backgroundRed
    cmp #0
    beq JoypadY  ; Value saturated.
    dec backgroundRed

JoypadY:
    lda joy1 + 1
    bit #$40  ; Y
    beq JoypadX  ; Button not pressed.
    lda backgroundGreen
    cmp #0
    beq JoypadX  ; Value saturated.
    dec backgroundGreen

JoypadX:
    lda joy1
    bit #$40  ; X
    beq JoypadL  ; Button not pressed.
    lda backgroundGreen
    cmp #31
    beq JoypadL  ; Value saturated.
    inc backgroundGreen

JoypadL:
    lda joy1
    bit #$20  ; L
    beq JoypadR  ; Button not pressed.
    lda backgroundBlue
    cmp #0
    beq JoypadR  ; Value saturated.
    dec backgroundBlue

JoypadR:
    lda joy1
    bit #$10  ; R
    beq JoypadB  ; Button not pressed.
    lda backgroundBlue
    cmp #31
    beq JoypadB  ; Value saturated.
    inc backgroundBlue

JoypadB:
    lda joy1 + 1
    bit #$80  ; B
    beq JoypadDone
    jsr MaybeShoot

JoypadDone:
    rts



MaybeShoot:
    ; If the cooldown timer is non-zero, don't shoot.
    lda shotCooldown
    cmp #0
    bne MaybeShootDone
    ; Find the first empty spot in the shots array.
    ldx #shotArray
-
    lda 0, X
    cmp #0
    beq +
    .rept shotSize
        inx
    .endr
    ; If we went all the way to the end, bail out.
    cpx #(shotArray + shotArrayLength * shotSize)
    beq MaybeShootDone
    jmp -
+
    ; Enable shot; set its position based on player position.
    ; TODO(mcmillen): it might be easier/faster to keep N arrays: one for each
    ; field of shot (shotSpriteArray, shotXArray, shotYArray, ...)
    lda #8  ; Sprite number.
    sta 0, X

    lda playerX
    adc #20
    sta 1, X

    lda playerY
    sta 2, X

    ; x-velocity.
    lda #6
    sta 3, X

    ; y-velocity.
    lda nextShotState
    cmp #1
    beq +
    lda #2
    sta 4, X
    inc nextShotState
    jmp ++
+
    lda #-2
    sta 4, X
    dec nextShotState
++

    ; Set cooldown timer.
    lda #10
    sta shotCooldown
MaybeShootDone:
    rts



UpdateWorld:
    ; Update shot cooldown.
    lda shotCooldown
    cmp #0
    beq +
    dec A
    sta shotCooldown
+

    ldx #0
    ; Update shot position.
UpdateShot:
    lda shotArray, X
    cmp #0
    beq ShotDone
    ; Add to the x-coordinate. If the carry bit is set, we went off the edge
    ; of the screen, so disable the shot.
    lda shotArray + 3, X  ; x-velocity.
    sta $00
    bit #%10000000  ; Check whether the velocity is negative.
    bne UpdateShotWithNegativeXVelocity
    lda shotArray + 1, X
    clc
    adc $00
    bcs DisableShot
    sta shotArray + 1, X  ; Store new x-coord.
    jmp UpdateShotY

UpdateShotWithNegativeXVelocity:
    ; TODO(mcmillen): wrap sprites when they go negative here, like we do
    ; with y-velocities.
    lda shotArray + 1, X  ; Current x.
    clc
    adc $00
    bcc DisableShot
    sta shotArray + 1, X
    jmp UpdateShotY

UpdateShotY:
    ; Add to the y-coordinate.
    lda shotArray + 4, X  ; y-velocity.
    sta $00
    bit #%10000000  ; Check whether the velocity is negative.
    bne UpdateShotWithNegativeYVelocity

    lda shotArray + 2, X
    adc $00
    cmp #224
    bcs DisableShot
    sta shotArray + 2, X  ; Store new y-coord.
    jmp ShotDone

UpdateShotWithNegativeYVelocity:
    lda shotArray + 2, X  ; Current y.
    cmp #224
    bcs +  ; If the shot was "off the top" before moving, maybe we'll reap it.
    adc $00  ; Otherwise, just update it,
    sta shotArray + 2, X  ; save the result,
    jmp ShotDone  ; and we know it shouldn't be reaped.
+
    adc $00
    dec A  ; Two's complement means that we need to -1 again in this case.
    cmp #224
    bcc DisableShot  ; If it's now wrapped around, reap it.
    sta shotArray + 2, X
    jmp ShotDone

DisableShot:
    stz shotArray, X

ShotDone:
    ; TODO(mcmillen): in places where we .rept inx (etc), is it faster to use
    ; actual addition?
    .rept shotSize
        inx
    .endr
    cpx #(shotArrayLength * shotSize)
    bne UpdateShot

    ; Make the background scroll. Horizontal over time; vertical depending on
    ; player's y-coordinate.
    lda vBlankCounter
    sta BG3HOFS
    lda vBlankCounter + 1
    sta BG3HOFS
    lda playerY
    .rept 3
        lsr
    .endr
    sta BG3VOFS
    stz BG3VOFS

    rts



UpdateSprites:
    ; Zero out the scratch space for the secondary sprite table.
    ldx #0
-
    stz spriteTableScratchStart, X
    inx
    cpx #numSprites
    bne -

    ldx #0  ; Index into sprite table 1.
    ldy #0  ; Index into sprite table 2.

    ; Copy player coords into sprite table.
    lda playerX
    sta spriteTableStart, X
    lda playerY
    sta spriteTableStart + 1, X
    lda #0
    sta spriteTableStart + 2, X
    ; Set priority bits so that the sprite is drawn in front.
    lda #%00110000
    sta spriteTableStart + 3, X
    lda #%11000000  ; Enable large sprite.
    sta spriteTableScratchStart, Y

    .rept 4
        inx
    .endr
    iny

    ; Now add shots.
    sty $00  ; Save sprite table 2 index.
    ldy #0  ; Index into shotArray.
-
    lda shotArray, Y
    cmp #0
    beq +  ; If not enabled, skip to next shot.
    ; Update sprite table 1.
    sta spriteTableStart + 2, X  ; sprite number
    lda shotArray + 1, Y
    sta spriteTableStart, X  ; x
    lda shotArray + 2, Y
    sta spriteTableStart + 1, X  ; y
    ; Update secondary sprite table.
    phy  ; Save shotArray index.
    ldy $00
    lda #%11000000
    sta spriteTableScratchStart, Y
    iny
    sty $00
    ply  ; Restore shotArrayIndex.

    .rept 4
        inx
    .endr
+
    .rept shotSize
        iny
    .endr
    cpy #(shotArrayLength * shotSize)
    bne -

    ; Now clear out the unused entries in the sprite table.
-
    cpx #spriteTable1Size
    beq +
    lda #1
    sta spriteTableStart, X
    .rept 4
        inx
    .endr
+
    rts



FillSecondarySpriteTable:
    ; The secondary sprite table wants 2 bits for each sprite: one to set the
    ; sprite's size, and one that's the high bit of the sprite's x-coordinate.
    ; It's annoying to deal with bitfields when thinking about business logic,
    ; so the spriteTableScratch array contains one byte for each sprite, in
    ; which the two most significant bits are the "size" and "upper x" bits.
    ; This function is meant to be called after UpdateWorld, and packs those
    ; bytes into the actual bitfield that the OAM wants for the secondary
    ; sprite table.
    ;
    ; The expected format of every byte in the scratch sprite table is:
    ; sx------    s = size (0 = small, 1 = large)
    ;             x = flipped high x-coordinate (so 1 behaves like "enable").
    ldx #0  ; Index into input table.
    ldy #0  ; Index into output table.
-
    stz $00  ; Current byte; filled out by a set of 4 input table entries.
    .rept 4
        ; For each byte, the lower-order bits correspond to the lower-numbered
        ; sprites; therefore we insert the current sprite's bits "at the top"
        ; and shift them right for each successive sprite.
        lsr $00
        lsr $00
        lda spriteTableScratchStart, X
        ora $00
        sta $00
        inx
    .endr
    lda $00
    eor #%01010101
    sta spriteTable2Start, Y
    iny
    cpx #numSprites
    bne -
    rts



SetBackgroundColor:
    ; The background-color bytes are (R, G, B), each ranging from [0-31].
    ; The palette color format is 15-bit: [0bbbbbgg][gggrrrrr]

    ; Set the background color.
    ; Entry 0 corresponds to the SNES background color.
    stz CGADDR

    ; Compute and the low-order byte and store it in CGDATA.
    lda backgroundGreen
    .rept 5
        asl
    .endr
    ora backgroundRed
    sta CGDATA

    ; Compute the high-order byte and store it in CGDATA.
    lda backgroundBlue
    .rept 2
        asl
    .endr
    sta $00
    lda backgroundGreen
    .rept 3
        lsr
    .endr
    ora $00
    sta CGDATA
    rts



VBlankHandler:
    jsr VBlankCounter
    jsr DMASpriteTables
    rti



VBlankCounter:
    ; Increment a counter of how many VBlanks we've done.
    ; This is a 24-bit counter. At 60 vblanks/second, this will take
    ; 77 hours to wrap around; that's good enough for me :)
    inc vBlankCounter
    bne +
    inc vBlankCounter + 1
    bne +
    inc vBlankCounter + 2
+
    rts



DMASpriteTables:
    ; Store at the base OAM address.
    ldx #$0000
    stx OAMADDR
    ; Default DMA control; destination $2104 (OAM data register).
    stz DMA0CTRL
    lda #$04
    sta DMA0DST
    ; Our sprites start at $0100 in bank 0 and are #$220 bytes long.
    ldx #spriteTableStart
    stx DMA0SRC
    stz DMA0SRCBANK
    ldx #spriteTableSize
    stx DMA0SIZE
    ; Kick off the DMA transfer.
    lda #%00000001
    sta DMAENABLE
    rts



.ENDS



; Bank 1 is used for our graphics assets.
.BANK 1 SLOT 0
.ORG 0
.SECTION "GraphicsData"

SpriteData:
    .INCBIN "sprites32.pic"
SpritePalette:
    .INCBIN "sprites32.clr"

TileData:
    .INCBIN "tiles.pic"
TilePalette:
    .INCBIN "tiles.clr"

.ENDS



; Fill an entire bank with random numbers.
.SEED 1
.BANK 2 SLOT 0
.ORG 0
.SECTION "RandomBytes"
.DBRND 32 * 1024, 0, 255
.ENDS
