.INCLUDE "header.asm"
.INCLUDE "init.asm"
.INCLUDE "registers.asm"
.INCLUDE "memory.asm"



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
    InitSNES

    ; By default we assume 16-bit X/Y and 8-bit A.
    ; If any code wants to change this, it's expected to do so itself,
    ; and to change them back to the defaults before returning.
    SetXY16Bit
    SetA8Bit

    jsr LoadPaletteAndTileData
    jsr InitWorld

    ; Set screen mode: 16x16 tiles for backgrounds, mode 1.
    lda #%11000001
    sta BGMODE

    ; Set sprite size to 8x8 (small) and 32x32 (large).
    lda #%00100000
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



InitWorld:
    ; Start the background color as a dark blue.
    lda #4
    sta backgroundBlue

    ; Player's initial starting location and health.
    lda #(256 / 4)
    sta playerX
    lda #((224 - 32) / 2)
    sta playerY
    lda #20
    sta playerHealth

    ; (x-velocity, y-velocity) of 4 different player shot patterns.
    lda #6
    sta shotVelocityTable
    lda #0
    sta shotVelocityTable + 1

    lda #3
    sta shotVelocityTable + 2
    lda #3
    sta shotVelocityTable + 3

    lda #0
    sta shotVelocityTable + 4
    lda #6
    sta shotVelocityTable + 5

    lda #-3
    sta shotVelocityTable + 6
    lda #3
    sta shotVelocityTable + 7

    lda #-6
    sta shotVelocityTable + 8
    lda #0
    sta shotVelocityTable + 9

    lda #-3
    sta shotVelocityTable + 10
    lda #-3
    sta shotVelocityTable + 11

    lda #0
    sta shotVelocityTable + 12
    lda #-6
    sta shotVelocityTable + 13

    lda #3
    sta shotVelocityTable + 14
    lda #-3
    sta shotVelocityTable + 15

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
    bra MainLoop



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
    ldx #playerShotArray
-
    lda 0, X
    cmp #0
    beq +
    .rept shotSize
        inx
    .endr
    ; If we went all the way to the end, bail out.
    cpx #(playerShotArray + playerShotArrayLength * shotSize)
    beq MaybeShootDone
    bra -
+
    ; Enable shot; set its position based on player position.
    ; TODO(mcmillen): it might be easier/faster to keep N arrays: one for each
    ; field of shot (shotSpriteArray, shotXArray, shotYArray, ...)
    lda #8  ; Sprite number.
    sta 0, X

    lda playerX
    clc
    adc #28
    sta 1, X

    lda playerY
    clc
    adc #14
    sta 2, X

    ; Get x- and y-velocity out of shotVelocityTable.
    lda nextShotState
    and #%00000000  ; 8 possibilities if we use #%00000111.
    ldy #0
-
    cmp #0
    beq +
    .rept 2
        iny
    .endr
    dec A
    bra -
+
    inc nextShotState

    ; x-velocity.
    lda shotVelocityTable, Y
    sta 3, X
    ; y-velocity.
    lda shotVelocityTable + 1, Y
    sta 4, X

    ; Set cooldown timer.
    lda #8
    sta shotCooldown
MaybeShootDone:
    rts



UpdateWorld:
    jsr UpdateShotCooldown
    jsr SpawnEnemyShips
    jsr SpawnEnemyShots
    jsr UpdateEnemyShips
    jsr UpdateShotPositions
    jsr CheckCollisionsWithPlayer
    jsr UpdateBackgroundScroll
    rts



UpdateShotCooldown:
    ; Update shot cooldown.
    lda shotCooldown
    cmp #0
    beq +
    dec A
    sta shotCooldown
+
    rts



SpawnEnemyShips:
    GetRandomByte
    bit #%01111111  ; Spawn ships every this-many frames (on average).
    beq +
    rts
+
    ; Find an empty spot in the array.
    ldy #0
-
    lda enemyShipArray, Y
    cmp #0
    beq +
    .rept enemyShipSize
        iny
    .endr
    cpy #(enemyShipArrayLength * enemyShipSize)
    bne -
    rts  ; Too many ships; bail.

+
    lda #4  ; Sprite number.
    sta enemyShipArray, Y

    lda #(256 - 32)
    sta enemyShipArray + 1, Y  ; x.

-
    GetRandomByte
    cmp #(224 - 32)
    bcs -  ; Keep trying.
    sta enemyShipArray + 2, Y  ; y.

    lda #0
    sta enemyShipArray + 3, Y  ; move AI type.
    sta enemyShipArray + 4, Y  ; shoot AI type.

    lda #8
    sta enemyShipArray + 5, Y  ; shot cooldown.

    rts



; TODO(mcmillen): reap ships if they move off the top, bottom, or right too.
UpdateEnemyShips:
    ldy #0
--
    lda enemyShipArray, Y
    cmp #0
    beq ++

    lda enemyShipArray + 1, Y  ; x
    clc
    adc #-2
    bcs +
    lda #0
    sta enemyShipArray, Y  ; reap it.
    bra ++
+
    sta enemyShipArray + 1, Y  ; move it.


++
    .rept enemyShipSize
        iny
    .endr

    cpy #(enemyShipArrayLength * enemyShipSize)
    bne --
    rts



SpawnEnemyShots:
    lda vBlankCounter
    bit #%00011111  ; Spawn shots every this-many frames.
    beq +
    rts
+
    ; Find an empty spot in the array.
    ldy #0
-
    lda enemyShotArray, Y
    cmp #0
    beq +
    .rept 6
        iny
    .endr
    cpy #(enemyShotArrayLength * shotSize)
    bne -
    rts  ; Too many shots; bail.

+
    lda #9  ; Sprite number.
    sta enemyShotArray, Y

    lda #254
    sta enemyShotArray + 1, Y  ; x.

    lda #((224 - 32) / 2)
    and #%01111111
    sta enemyShotArray + 2, Y  ; y.

    lda #-4
    sta enemyShotArray + 3, Y  ; x-velocity.

    GetRandomByte
    and #%00000111  ; [0, 7]
    clc
    adc #-3  ; [-3, 4]
    cmp #4
    bne +
    lda #0  ; [-3, 3] with 2x chance of zero.
+
    sta enemyShotArray + 4, Y  ; y-velocity.
    rts



UpdateShotPositions:
    ldx #0

UpdateShot:  ; Updates position of one shot.
    lda playerShotArray, X
    cmp #0
    beq ShotDone
    ; Add to the x-coordinate. If the carry bit is set, we went off the edge
    ; of the screen, so disable the shot.
    lda playerShotArray + 3, X  ; x-velocity.
    sta $00
    bit #%10000000  ; Check whether the velocity is negative.
    bne UpdateShotWithNegativeXVelocity
    lda playerShotArray + 1, X
    clc
    adc $00
    bcs DisableShot
    sta playerShotArray + 1, X  ; Store new x-coord.
    bra UpdateShotY

UpdateShotWithNegativeXVelocity:
    ; TODO(mcmillen): wrap sprites when they go negative here, like we do
    ; with y-velocities.
    lda playerShotArray + 1, X  ; Current x.
    clc
    adc $00
    bcc DisableShot
    sta playerShotArray + 1, X

UpdateShotY:
    ; Add to the y-coordinate.
    lda playerShotArray + 4, X  ; y-velocity.
    sta $00
    bit #%10000000  ; Check whether the velocity is negative.
    bne UpdateShotWithNegativeYVelocity

    lda playerShotArray + 2, X
    clc
    adc $00
    cmp #224
    bcs DisableShot
    sta playerShotArray + 2, X  ; Store new y-coord.
    bra ShotDone

UpdateShotWithNegativeYVelocity:
    lda playerShotArray + 2, X  ; Current y.
    cmp #224
    bcs +  ; If the shot was "off the top" before moving, maybe we'll reap it.
    adc $00  ; Otherwise, just update it,
    sta playerShotArray + 2, X  ; save the result,
    bra ShotDone  ; and we know it shouldn't be reaped.
+
    clc
    adc $00
    cmp #224
    bcc DisableShot  ; If it's now wrapped around, reap it.
    sta playerShotArray + 2, X
    bra ShotDone

DisableShot:
    stz playerShotArray, X

ShotDone:
    ; TODO(mcmillen): in places where we .rept inx (etc), is it faster to use
    ; actual addition?
    .rept shotSize
        inx
    .endr
    cpx #((playerShotArrayLength + enemyShotArrayLength) * shotSize)
    bne UpdateShot

    rts



CheckCollisionsWithPlayer:
    ; Store player position statically.
    clc
    lda playerX
    adc #16  ; Can't overflow.
    sta $00  ; Store the center.
    lda playerY
    ; Store the center. Our ship is actually 31 pixels tall, so offsetting by
    ; 15 feels more "fair": a shot that hits the invisible bottom edge of the
    ; ship won't count as a hit.
    adc #15
    sta $01

    ldx #0
--
    lda enemyShotArray, X
    cmp #0  ; Check whether it's active.
    beq ++

    ; Find dx.
    lda enemyShotArray + 1, X  ; x.
    clc
    adc #2  ; Get the center of the shot.
    sbc $00
    bpl +  ; If the result is positive, great!
    eor #$ff  ; Otherwise, negate it.
    inc A
+
    ; A now contains dx, guaranteed to be positive.
    cmp #18  ; Threshold for "successful hit".
    bcs ++  ; Already too far; bail.
    sta $02

    ; Find dy.
    lda enemyShotArray + 2, X  ; y.
    clc
    adc #2
    sbc $01
    bpl +  ; If the result is positive, great!
    eor #$ff  ; Otherwise, negate it.
    inc A
+
    ; A now contains dy, guaranteed to be positive.
    clc
    adc $02  ; Add dx.
    cmp #18  ; Threshold for "successful hit".
    bcs ++

    ; OK, we got a hit!
    ; Disable the shot.
    lda #0
    sta enemyShotArray, X

    ; And decrement the player's life.
    lda playerHealth
    cmp #0
    beq ++
    dec playerHealth

++
    .rept shotSize
        inx
    .endr

    cpx #(enemyShotArrayLength * shotSize)
    bne --
    rts



UpdateBackgroundScroll:
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



UpdateSprites:  ; TODO(mcmillen): refactor into smaller pieces.
    ; This page is a good reference on SNES sprite formats:
    ; http://wiki.superfamicom.org/snes/show/SNES+Sprites
    ; It uses the same approach we're using, in which we keep a buffer of the
    ; sprite tables in RAM, and DMA the sprite tables to the system's OAM
    ; during VBlank.
    ; Sprite table 1 has 4 bytes per sprite, laid out as follows:
    ; Byte 1:    xxxxxxxx    x: X coordinate
    ; Byte 2:    yyyyyyyy    y: Y coordinate
    ; Byte 3:    cccccccc    c: Starting tile #
    ; Byte 4:    vhoopppc    v: vertical flip h: horizontal flip  o: priority bits
    ;                        p: palette #
    ; Sprite table 2 has 2 bits per sprite, like so:
    ; bits 0,2,4,6 - High bit of the sprite's x-coordinate.
    ; bits 1,3,5,7 - Toggle Sprite size: 0 - small size   1 - large size
    ; Setting all the high bits keeps the sprites offscreen.

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
    lda #%00010000
    sta spriteTableStart + 3, X
    lda #%11000000  ; Enable large sprite.
    sta spriteTableScratchStart, Y

    .rept 4
        inx
    .endr
    iny

    ; Now add enemy ships.
    sty $00  ; Save sprite table 2 index.
    ldy #0  ; Index into enemyShipArray.
-
    lda enemyShipArray, Y
    cmp #0  ; If not enabled, skip to next ship.
    beq +
    ; Update sprite table 1.
    sta spriteTableStart + 2, X  ; sprite number
    lda enemyShipArray + 1, Y
    sta spriteTableStart, X  ; x
    lda enemyShipArray + 2, Y
    sta spriteTableStart + 1, X  ; y
    lda #%01000000  ; flip horizontally.
    sta spriteTableStart + 3, X
    ; Update secondary sprite table.
    phy  ; Save enemyShipArray index.
    ldy $00
    lda #%11000000  ; Enable large sprite.
    sta spriteTableScratchStart, Y
    iny
    sty $00
    ply  ; Restore enemyShipArray index.

    .rept 4
        inx
    .endr

+
    .rept enemyShipSize
        iny
    .endr
    cpy #(enemyShipArrayLength * enemyShipSize)
    bne -
    ldy $00  ; Restore Y to its rightful self.

    ; Now add shots.
    sty $00  ; Save sprite table 2 index.
    ldy #0  ; Index into playerShotArray.
-
    lda playerShotArray, Y
    cmp #0
    beq +  ; If not enabled, skip to next shot.
    ; Update sprite table 1.
    sta spriteTableStart + 2, X  ; sprite number
    lda playerShotArray + 1, Y
    sta spriteTableStart, X  ; x
    lda playerShotArray + 2, Y
    sta spriteTableStart + 1, X  ; y
    ; Update secondary sprite table.
    phy  ; Save playerShotArray index.
    ldy $00
    lda #%01000000  ; Enable small sprite.
    sta spriteTableScratchStart, Y
    iny
    sty $00
    ply  ; Restore playerShotArray index.

    .rept 4
        inx
    .endr
+
    .rept shotSize
        iny
    .endr
    cpy #((playerShotArrayLength + enemyShotArrayLength) * shotSize)
    bne -
    ldy $00  ; Restore Y to its rightful self.

    ; Now add sprites to show player health.
    ; TODO(mcmillen): why aren't they in front?
    stz $01
    lda #4
    sta $02
-
    lda $01
    cmp playerHealth
    beq +  ; All done?
    lda #10
    sta spriteTableStart + 2, X  ; sprite number
    lda $02
    sta spriteTableStart, X  ; x
    clc
    adc #7
    sta $02
    lda #212
    sta spriteTableStart + 1, X  ; y
    ; Set priority bits so that the sprite is drawn in front.
    lda #%00110000
    sta spriteTableStart + 3, X
    lda #%01000000  ; Enable small sprite.
    sta spriteTableScratchStart, Y
    .rept 4
        inx
    .endr
    iny
    inc $01
    bra -
+

    ; Now clear out the unused entries in the sprite table.
-
    cpx #spriteTable1Size
    beq +
    lda #1
    sta spriteTableStart, X
    .rept 4
        inx
    .endr
    bra -
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
