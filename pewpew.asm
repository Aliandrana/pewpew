.INCLUDE "header.asm"
.INCLUDE "InitSNES.asm"
.INCLUDE "registers.asm"

.BANK 0 SLOT 0
.ORG 0
.SECTION "MainCode"


; Memory layout:
; 00-0F: scratch space for functions.
; 10-13: controller state.
; 14-17: 32-bit counter of vblanks.
; 20-22: RGB color values to use for background color, from [0-31].


Start:
    InitSNES  ; Initialize SNES.
                      
    ; Turn on the screen. 
    ; Format: x000bbbb 
    ; x: 0 = screen on, 1 = screen off, bbbb: Brightness ($0-$F)
    lda #%00001111
    sta INIDISP

    ; Enable NMI interrupt & joypad.
    ; n-vh---j   n: NMI interrupt enable         v: vertical counter enable
    ;            h: horizontal counter enable    j: joypad enable
    lda #$81
    sta NMITIMEN

    ; Store zeroes to the controller status registers.
    ; TODO(mcmillen): is this needed? I think the system will overwrite these
    ; automatically.
    stz JOY1H
    stz JOY1L

    ; Write something recognizable into our scratch space.
    jsr FillScratch



MainLoop:
    wai  ; Wait for interrupt.
    jmp MainLoop



VBlankHandler:
    jsr VBlankCounter  ; DEBUG
    jsr JoypadHandler
    jsr SetBackgroundColor
    ; jsr FillScratch  ; DEBUG
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
    ; $4218: Joypad #1 status [JOY1L]
    ; Format: AXLR0000
    ; $4219: Joypad #1 status [JOY1H]
    ; Format: BYsSudlr (s=select, S=start, udlr = joypad)
    jsr JoypadDebug  ; DEBUG

; TODO(mcmillen): read joypad from local memory instead of registers?
JoypadUp:
    lda JOY1H
    and #$08  ; Up
    cmp #$08
    bne JoypadDown  ; Button not pressed.
    lda $20
    cmp #31
    beq JoypadDown  ; Value saturated.
    inc $20

JoypadDown:
    lda JOY1H
    and #$04
    cmp #$04
    bne JoypadLeft  ; Button not pressed.
    lda $20
    cmp #0
    beq JoypadLeft  ; Value saturated.
    dec $20

JoypadLeft:
    lda JOY1H
    and #$02  ; Left
    cmp #$02
    bne JoypadRight  ; Button not pressed.
    lda $22
    cmp #0
    beq JoypadRight  ; Value saturated.
    dec $22

JoypadRight:
    lda JOY1H
    and #$01
    cmp #$01  ; Right
    bne JoypadB  ; Button not pressed.
    lda $22
    cmp #31
    beq JoypadB  ; Value saturated.
    inc $22

JoypadB:
    lda JOY1H
    and #$80  ; B
    cmp #$80
    bne JoypadY  ; Button not pressed.
    lda $21
    cmp #31
    beq JoypadY  ; Value saturated.
    inc $21

JoypadY:
    lda JOY1H
    and #$40  ; Y
    cmp #$40
    bne JoypadDone  ; Button not pressed.
    lda $21
    cmp #0
    beq JoypadDone  ; Value saturated.
    dec $21

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
    ; $20 $21 $22 are (R, G, B), each ranging from [0-31].
    ; The palette color format is 15-bit: [0bbbbbgg][gggrrrrr]
    
    ; Compute and the low-order byte and store it in CGDATA.
    lda $21  ; Green.
    .rept 5
        asl
    .endr
    ora $20  ; Red.
    sta CGDATA

    ; Compute the high-order byte and store it in CGDATA.
    lda $22  ; Blue.
    .rept 2
        asl
    .endr
    sta $00
    lda $21  ; Green.
    .rept 3
        lsr
    .endr
    ora $00
    sta CGDATA

    ; Set the background color.
    ; $2121 is the color palette selection register [CGADD].
    ; Entry 0 corresponds to the SNES background color.
    stz CGADD
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
