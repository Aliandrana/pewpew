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
; 00-0F: scratch space for functions.
; 10-11: controller state of joypad #1.
; 12-13: controller state of joypad #2.
; 14-17: 32-bit counter of vblanks.
; 20-21: (x, y) coordinates of player.
; 22-24: RGB color values to use for background color, from [0-31].


Start:
    InitializeSNES

    jsr LoadPaletteAndTileData
                    
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
    lda #32  ; Palette entries for BG2 start at 32.
    sta CGADDR
-    
    lda.l TilePalette, x
    sta CGDATA
    inx
    cpx #8  ; 8 bytes of palette data.
    bne -

    ; DMA 0 source address & bank.
    ldx #TileData
    stx DMA0SRC
    lda #:TileData
    sta DMA0SRCBANK
    ; DMA 0 transfer size.
    ; See the helpful comment in tiles.asm to find the size of the tile data.
    ldy #384  
    sty DMA0SIZE
    ; DMA 0 control register.
    ; Transfer type 001 = 2 addresses, LH.
    lda #%00000001
    sta DMA0CTRL
    ; DMA 0 destination.
    lda #$18  ; Upper-byte is assumed to be $21, so this is $2118 & $2119.
    sta DMA0DST
    ; $2116 sets the word address for accessing VRAM.
    ldy #$0000
    sty VMADDR
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

    ; Main screen: enable BG2.
    lda #%00000010
    sta MSENABLE

    rts



VBlankHandler:
    jsr VBlankCounter  ; DEBUG
    jsr JoypadHandler
    jsr SetBackgroundColor
    jsr SetPlayerPosition
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
    ; Make sure the high byte of A is zeroed out.
    lda #$00
    xba
    ; Get player x and convert it to a scroll offset.
    lda $0020
    ConvertXCoordinate
    sta BG2HOFS
    xba
    sta BG2HOFS

    ; Make sure the high byte of A is zeroed out.
    lda #$00
    xba
    ; Get player y and convert it to a scroll offset.
    lda $0021
    ConvertYCoordinate
    sta BG2VOFS
    xba
    sta BG2VOFS
    rts



FillScratch:
    lda #$42  ; B
    ldx #0
FillScratchLoop:
    sta $00,X
    inx
    cpx #$10
    bne FillScratchLoop
    rts

.ENDS



.BANK 1 SLOT 0
.ORG 0
.SECTION "TileData"
.INCLUDE "tiles.asm"
.ENDS
