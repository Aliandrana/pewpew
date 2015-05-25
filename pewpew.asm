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
; 0030-0032: enable / x / y of shot.
;
; Sprite table buffers -- copied each frame to OAM during VBlank, using DMA.
; 0100-02FF: table 1 (4 bytes each: x/y coord, tile #, flip/priority/palette)
; 0300-031F: table 2 (2 bits each: high x-coord bit, size)
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
.define shotData $30

; TODO(mcmillen): verify that we can relocate these without messing things up.
.define spriteTableStart $100
.define spriteTable1Size $200
.define spriteTable2Start $300  ; TODO(mcmillen): use this.
.define spriteTableSize $220


; Stores result to A.
; Assumes 16-bit X & 8-bit A.
; Modifies X.
; Updates randomBytePtr.
.MACRO GetRandomByte
    ldx randomBytePtr
    lda $028000, X  ; $028000: beginning of ROM bank 2.
    inx
    cpx #$8000  ; This is the size of the entire ROM bank.
    bne +
    ldx #0
+
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
    rep #%00010000  ; 16-bit X/Y.
    sep #%00100000  ; 8-bit A/B.

    ; Store zeroes to the controller status registers.
    ; TODO(mcmillen): is this needed? I think the system will overwrite these
    ; automatically.
    stz JOY1H
    stz JOY1L

    jsr LoadPaletteAndTileData
    jsr InitializeSpriteTables
    jsr InitializeWorld

    ; Set screen mode: 16x16 tiles for backgrounds, mode 1.
    lda #%11000001
    sta SCREENMODE

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

    ; Enable NMI interrupt & joypad.
    ; n-vh---j   n: NMI interrupt enable         v: vertical counter enable
    ;            h: horizontal counter enable    j: joypad enable
    lda #%10000001
    sta NMITIMEN

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
    ; TODO(mcmillen): BG2 started at 32, but maybe that's only in mode 0?
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
    and #%00000111
    cmp #%00000111
    bne +
    ldx #$0002
    lda $00
    and #%10000000
    cmp #%10000000
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
    rep #%00100000  ; 16-bit A.

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
    inx
    inx
    inx
    inx
    cpx #spriteTable1Size
    bne -

    ; Fill sprite table 2.  2 bits per sprite, like so:
    ; bits 0,2,4,6 - High bit of the sprite's x-coordinate.
    ; bits 1,3,5,7 - Toggle Sprite size: 0 - small size   1 - large size
    ; Setting all the high bits keeps the sprites offscreen.
    lda #%0101010101010101
-
    sta spriteTableStart, X
    inx
    inx
    cpx #spriteTableSize
    bne -

    sep #%00100000  ; 8-bit A.
    rts



InitializeWorld:
    ; Start the background color as a dark blue.
    lda #4
    sta $24

    ; Player's initial starting location.
    lda #(256 / 4)
    sta playerX
    lda #((224 - 32) / 2)
    sta playerY
    rts



MainLoop:
    wai  ; Wait for interrupt.
    jsr JoypadDebug
    jsr JoypadHandler
    jsr UpdateWorld
    jsr SetBackgroundColor
    jmp MainLoop



JoypadDebug:
    ; Load joypad registers into RAM for easier inspection.
    ldx JOY1L
    stx joy1
    ldx JOY2L
    stx joy2
    rts



JoypadHandler:
; TODO(mcmillen): handle joystick using 16-bit loads?
JoypadUp:
    lda JOY1H
    and #$08  ; Up
    cmp #$08
    bne JoypadDown  ; Button not pressed.
    lda playerY
    cmp #0
    beq JoypadDown  ; Value saturated.
    dec playerY
    dec playerY

JoypadDown:
    lda JOY1H
    and #$04
    cmp #$04
    bne JoypadLeft  ; Button not pressed.
    lda playerY
    cmp #(224 - 32)
    beq JoypadLeft  ; Value saturated.
    inc playerY
    inc playerY

JoypadLeft:
    lda JOY1H
    and #$02  ; Left
    cmp #$02
    bne JoypadRight  ; Button not pressed.
    lda playerX
    cmp #0
    beq JoypadRight  ; Value saturated.
    dec playerX
    dec playerX

JoypadRight:
    lda JOY1H
    and #$01
    cmp #$01  ; Right
    bne JoypadStart  ; Button not pressed.
    lda playerX
    cmp #(256 - 32)
    beq JoypadStart  ; Value saturated.
    inc playerX
    inc playerX

JoypadStart:
    lda JOY1H
    and #$10  ; Start
    cmp #$10
    bne JoypadSelect  ; Button not pressed.
    lda backgroundRed
    cmp #0
    beq JoypadSelect  ; Value saturated.
    dec backgroundRed

JoypadSelect:
    lda JOY1H
    and #$20  ; Select
    cmp #$20
    bne JoypadY  ; Button not pressed.
    lda backgroundRed
    cmp #31
    beq JoypadY  ; Value saturated.
    inc backgroundRed

JoypadY:
    lda JOY1H
    and #$40  ; Y
    cmp #$40
    bne JoypadX  ; Button not pressed.
    lda backgroundGreen
    cmp #0
    beq JoypadX  ; Value saturated.
    dec backgroundGreen

JoypadX:
    lda JOY1L
    and #$40  ; X
    cmp #$40
    bne JoypadL  ; Button not pressed.
    lda backgroundGreen
    cmp #31
    beq JoypadL  ; Value saturated.
    inc backgroundGreen

JoypadL:
    lda JOY1L
    and #$20  ; L
    cmp #$20
    bne JoypadR  ; Button not pressed.
    lda backgroundBlue
    cmp #0
    beq JoypadR  ; Value saturated.
    dec backgroundBlue

JoypadR:
    lda JOY1L
    and #$10  ; R
    cmp #$10
    bne JoypadB  ; Button not pressed.
    lda backgroundBlue
    cmp #31
    beq JoypadB  ; Value saturated.
    inc backgroundBlue

JoypadB:
    lda JOY1H
    and #$80  ; B
    cmp #$80
    bne JoypadDone
    jsr MaybeShoot

JoypadDone:
    rts



MaybeShoot:
    ; If the cooldown timer is non-zero, don't shoot.
    lda shotCooldown
    cmp #0
    bne +
    ; Enable shot; set its position to player position.
    lda #1
    sta shotData
    lda playerX
    sta shotData + 1
    lda playerY
    sta shotData + 2
    ; Set cooldown timer.
    lda #16
    sta shotCooldown
+
    rts



UpdateWorld:
    ; Update shot cooldown.
    lda shotCooldown
    cmp #0
    beq +
    dea
    sta shotCooldown
+

    ; Copy player coords into sprite table.
    lda playerX
    sta $0100
    lda playerY
    sta $0101
    ; Set the sprite.
    lda #0
    sta $0102
    ; Set priority bits so that the sprite is drawn in front.
    lda #%00110000
    sta $0103
    ; Clear x-MSB so that the sprite is displayed.
    lda spriteTable2Start
    and #%11111110
    ora #%00000010  ; ... and make it the large size. (32x32)
    sta spriteTable2Start

    ; Move shot coords.
    ldx $0
    lda shotData
    cmp #1
    bne DisableShot

    lda shotData + 1
    ; TODO(mcmillen): do this with an add, then check the carry bit?
    .rept 6
        ina
        cmp #$00  ; If it wraps around, disable it.
        bne +
        stz shotData
+
    .endr
    sta shotData + 1  ; Store new x-coord.

    ; See if sprite is still enabled after move.
    lda shotData
    cmp #1
    bne DisableShot

    ; Set up shot sprite.
    lda shotData + 1  ; x
    sta $0104
    lda shotData + 2  ; y
    sta $0105
    lda #8     ; which sprite
    sta $0106

    lda $0300
    and #%11111011  ; Display it.
    ora #%00001000  ; and set it to large size.
    sta $0300
    jmp ShotDone

DisableShot:
    ; Disable it by setting x-position to 1 and setting the high x-bit.
    lda #1
    sta $104
    lda $0300
    ora #%00000100
    sta $0300

ShotDone:
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
    lda vBlankCounter
    cmp #$00
    bne VBlankCounterDone
    inc vBlankCounter + 1
    lda vBlankCounter + 1
    cmp #$00
    bne VBlankCounterDone
    inc vBlankCounter + 2
VBlankCounterDone:
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