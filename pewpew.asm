.INCLUDE "header.asm"
.INCLUDE "init.asm"
.INCLUDE "registers.asm"


; The JSR and RTS instructions add a total of 12 cycles of overhead.  For
; short, commonly-used functions, it makes sense to declare them as macros,
; which get inlined by the assembler at the point of use.  This saves on
; CPU cycles, at the cost of code size.


.MACRO ConvertXCoordinate
; Data in: world x-coordinate, in A register.
; Data out: SNES scroll data, in C (the 16-bit A register).
rep #%00100000  ; 16-bit A
eor #$FFFF  ; Flip bits
ina
sep #%00100000  ; 8-bit A
.ENDM



.MACRO ConvertYCoordinate
; Data in: world y-coordinate, in A register.
; Data out: SNES scroll data, in C (the 16-bit A register).
rep #%00100000  ; 16-bit A
eor #$FFFF  ; Flip bits
sep #%00100000  ; 8-bit A
.ENDM



.BANK 0 SLOT 0
.ORG 0
.SECTION "MainCode"


; Memory layout:
; 0000-000F: scratch space for functions.
; 0010-0011: controller state of joypad #1.
; 0012-0013: controller state of joypad #2.
; 0014-0017: 32-bit counter of vblanks.
; 0020-0021: (x, y) coordinates of player.
; 0022-0024: RGB color values to use for background color, from [0-31].
;
; Sprite table buffers -- copied each frame to OAM during VBlank, using DMA.
; 0100-02FF: table 1 (4 bytes each: x/y coord, tile #, flip/priority/palette)
; 0300-031F: table 2 (2 bits each: high x-coord bit, size)


Start:
    InitializeSNES

    jsr LoadPaletteAndTileData
    jsr InitializeSpriteTables

    ; Set sprite size to 16x16 (small) and 32x32 (large).
    lda #%01100000
    sta OAMSIZE

    ; Main screen: enable sprites.
    lda #%00010000
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

    ; Store zeroes to the controller status registers.
    ; TODO(mcmillen): is this needed? I think the system will overwrite these
    ; automatically.
    stz JOY1H
    stz JOY1L

    ; Write something recognizable into our scratch space.
    jsr FillScratch

    ; Start the background color as a sky blue.
    lda #16
    sta $23
    lda #31
    sta $24

    ; Player's initial starting location.
    lda #(256 / 4)
    sta $20
    lda #((224 - 8) / 2)
    sta $21



MainLoop:
    wai  ; Wait for interrupt.
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

    ; 16-bit X/Y registers. Used for DMA source address & transfer size, both of
    ; which want 16-bit values.
    rep #%00010000
    ; 8-bit A/B registers. Used for DMA source bank & destination address.
    sep #%00100000

    ; Initialize the palette memory in a loop.
    ; We could also do this with a DMA transfer (like we do with the tile data
    ; below), but it seems overkill for just a few bytes. :)
    ldx #0
    lda #128  ; Palette entries for sprites start at 128.
    sta CGADDR
-    
    lda.l SpritePalette, X
    sta CGDATA
    inx
    cpx #32  ; 32 bytes of palette data.
    bne -

    ; DMA 0 source address & bank.
    ldx #SpriteData
    stx DMA0SRC
    lda #:SpriteData
    sta DMA0SRCBANK
    ; DMA 0 transfer size.
    ; See the helpful comment in sprites.asm to find the size of the tile data.
    ldx #576
    stx DMA0SIZE
    ; DMA 0 control register.
    ; Transfer type 001 = 2 addresses, LH.
    lda #%00000001
    sta DMA0CTRL
    ; DMA 0 destination.
    lda #$18  ; Upper-byte is assumed to be $21, so this is $2118 & $2119.
    sta DMA0DST
    ; $2116 sets the word address for accessing VRAM.
    ldx #$0000
    stx VMADDR
    ; Enable DMA channel 0.
    lda #%00000001
    sta DMAENABLE

    ; VRAM writing mode. Increments the address every time we write to $2119.
    lda #%10000000
    sta VMAIN
    ; Set word address for accessing VRAM to $6000.
    ldx #$6000  ; BG 2 starts here.
    stx VMADDR
    ldx #$0004  ; Stick one tile into BG2.
    stx VMDATA

    ; Set up the screen. 16x16 tiles for BG2, 8x8 tiles elsewhere, mode 0.
    lda #%00100000
    sta BGMODE
    ; $2108 is the BG2 VRAM location register.
    ; This tells it that the BG2 data starts at $6000.
    lda #%01100000
    sta BG2SC
    stz BG12NBA

    rts



InitializeSpriteTables:
    ; This page is a good reference on SNES sprite formats:
    ; http://wiki.superfamicom.org/snes/show/SNES+Sprites
    ; It uses the same approach we're using, in which we keep a buffer of the
    ; sprite tables in RAM, and DMA the sprite tables to the system's OAM
    ; during VBlank.
    rep #%00110000  ; 16-bit A/X/Y.

    ldx #$0000
    ; Fill sprite table 1.  4 bytes per sprite, laid out as follows:
    ; Byte 1:    xxxxxxxx    x: X coordinate
    ; Byte 2:    yyyyyyyy    y: Y coordinate
    ; Byte 3:    cccccccc    c: Starting tile #
    ; Byte 4:    vhoopppc    v: vertical flip h: horizontal flip  o: priority bits
    ;                        p: palette #
    lda #$01
-
    sta $0100, X  ; We keep our sprite table at $0100 and DMA it to OAM later.
    inx
    inx
    inx
    inx
    cpx #$0200
    bne -

    ; Fill sprite table 2.  2 bits per sprite, like so:
    ; bits 0,2,4,6 - Enable or disable the X coordinate's 9th bit.
    ; bits 1,3,5,7 - Toggle Sprite size: 0 - small size   1 - large size
    lda #%0101010101010101
-
    sta $0100, X
    inx
    inx
    cpx #$0220
    bne -

    sep #%00100000  ; 8-bit A.
    rts



VBlankHandler:
    jsr VBlankCounter  ; DEBUG
    jsr JoypadHandler
    jsr SetBackgroundColor
    jsr SetPlayerPosition
    jsr DMASpriteTables
    rti



VBlankCounter:
    ; Increment a counter of how many VBlanks we've done.
    inc $14
    lda $14
    cmp #$00
    bne VBlankCounterDone
    inc $15
    lda $15
    cmp #$00
    bne VBlankCounterDone
    inc $16
    lda $16
    cmp #$00
    bne VBlankCounterDone
    inc $17
VBlankCounterDone:
    rts



JoypadHandler:
    jsr JoypadDebug  ; DEBUG

JoypadUp:
    lda JOY1H
    and #$08  ; Up
    cmp #$08
    bne JoypadDown  ; Button not pressed.
    lda $21
    cmp #0
    beq JoypadDown  ; Value saturated.
    dec $21
    dec $21

JoypadDown:
    lda JOY1H
    and #$04
    cmp #$04
    bne JoypadLeft  ; Button not pressed.
    lda $21
    cmp #(224 - 16)
    beq JoypadLeft  ; Value saturated.
    inc $21
    inc $21

JoypadLeft:
    lda JOY1H
    and #$02  ; Left
    cmp #$02
    bne JoypadRight  ; Button not pressed.
    lda $20
    cmp #0
    beq JoypadRight  ; Value saturated.
    dec $20
    dec $20

JoypadRight:
    lda JOY1H
    and #$01
    cmp #$01  ; Right
    bne JoypadB  ; Button not pressed.
    lda $20
    cmp #(256 - 16)
    beq JoypadB  ; Value saturated.
    inc $20
    inc $20

JoypadB:
    lda JOY1H
    and #$80  ; B
    cmp #$80
    bne JoypadA  ; Button not pressed.
    lda $22
    cmp #0
    beq JoypadA  ; Value saturated.
    dec $22

JoypadA:
    lda JOY1L
    and #$80  ; A
    cmp #$80
    bne JoypadY  ; Button not pressed.
    lda $22
    cmp #31
    beq JoypadY  ; Value saturated.
    inc $22

JoypadY:
    lda JOY1H
    and #$40  ; Y
    cmp #$40
    bne JoypadX  ; Button not pressed.
    lda $23
    cmp #0
    beq JoypadX  ; Value saturated.
    dec $23

JoypadX:
    lda JOY1L
    and #$40  ; X
    cmp #$40
    bne JoypadL  ; Button not pressed.
    lda $23
    cmp #31
    beq JoypadL  ; Value saturated.
    inc $23

JoypadL:
    lda JOY1L
    and #$20  ; L
    cmp #$20
    bne JoypadR  ; Button not pressed.
    lda $24
    cmp #0
    beq JoypadR  ; Value saturated.
    dec $24

JoypadR:
    lda JOY1L
    and #$10  ; R
    cmp #$10
    bne JoypadDone  ; Button not pressed.
    lda $24
    cmp #31
    beq JoypadDone  ; Value saturated.
    inc $24

; TODO(mcmillen): have Start and Select do something too.

JoypadDone:
    rts  



JoypadDebug:
    ; Load joypad registers into RAM for easier inspection.
    lda JOY1L
    sta $10
    lda JOY1H
    sta $11
    lda JOY2L
    sta $12
    lda JOY2H
    sta $13
    rts



SetBackgroundColor:
    ; $22 $23 $24 are (R, G, B), each ranging from [0-31].
    ; The palette color format is 15-bit: [0bbbbbgg][gggrrrrr]
    
    ; Set the background color.
    ; Entry 0 corresponds to the SNES background color.
    stz CGADDR

    ; Compute and the low-order byte and store it in CGDATA.
    lda $23  ; Green.
    .rept 5
        asl
    .endr
    ora $22  ; Red.
    sta CGDATA

    ; Compute the high-order byte and store it in CGDATA.
    lda $24  ; Blue.
    .rept 2
        asl
    .endr
    sta $00
    lda $23  ; Green.
    .rept 3
        lsr
    .endr
    ora $00
    sta CGDATA
    rts



SetPlayerPosition:
    ; Copy player coords into sprite table.
    lda $0020
    sta $0100
    lda $0021
    sta $0101
    ; Clear x-MSB so that the sprite is displayed.
    lda $0300
    and #%11111110
    sta $0300
    rts



DMASpriteTables:
    rep #%00010000  ; 16-bit X/Y.
    sep #%00100000  ; 8-bit A.
    ; Store at the base OAM address.
    ldx #$0000
    stx OAMADDR
    ; Default DMA control; destination $2104 (OAM data register).
    stz DMA0CTRL
    lda #$04
    sta DMA0DST
    ; Our sprites start at $0100 in bank 0 and are #$220 bytes long.
    ldx #$0100
    stx DMA0SRC
    stz DMA0SRCBANK
    ldx #$0220
    stx DMA0SIZE
    ; Kick off the DMA transfer.
    lda #%00000001
    sta DMAENABLE
    rts



FillScratch:
    lda #$42  ; ASCII "B"
    ldx #0
-
    sta $00, X
    inx
    cpx #$10
    bne -
    rts


.ENDS



.BANK 1 SLOT 0
.ORG 0
.SECTION "SpriteData"
.INCLUDE "sprites.asm"
.ENDS
