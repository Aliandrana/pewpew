.INCLUDE "header.inc"
.INCLUDE "InitSNES.asm"

; Memory layout:
; 00-0F: scratch space for functions.
; 10-13: controller state.
; 14-17: 32-bit counter of vblanks.
; 20-22: [rgb] color values to use for background color.

;========================
; Start
;========================

.BANK 0 SLOT 0
.ORG 0
.SECTION "MainCode"


Start:
    InitSNES            ; Initialize SNES.

    ; Set the background color.
    ; $2121 is the color palette selection register [CGADD].
    ; Storing 0 because that's the SNES background color.
    stz $2121
                        
    ; Turn on the screen. 
    ; $2100: Screen display register [INIDISP]
    ;
    ; Format: x000bbbb 
    ; x: 0 = screen on, 1 = screen off, bbbb: Brightness ($0-$F)
    lda #%00001111
    sta $2100

    ; Enable NMI interrupt & joypad.
    ; Register $4200: Counter enable [NMITIMEN]
    ; n-vh---j   n: NMI interrupt enable         v: vertical counter enable
    ;            h: horizontal counter enable    j: joypad enable
    lda #$81
    sta $4200

    ; Store zeroes to the controller status registers.
    ; TODO(mcmillen): is this needed?
    stz $4218
    stz $4219

    ; Write something recognizable into our scratch space.
    jsr FillScratch


Loop:
    wai
    jmp Loop


VBlankHandler:
    jsr VBlankCounter  ; DEBUG
    jsr JoypadHandler
    jsr SetBackgroundColor
    ; jsr FillScratch  ; DEBUG
    rti


VBlankCounter:
    ; Keeps a counter of how many VBlanks we've done.
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
    ; Joypad #2 status is $421A [JOY2L] and $421B [JOY2H].
    jsr JoypadDebug  ; DEBUG

; TODO(mcmillen): read joypad from local memory instead of registers?
JoypadUp:
    lda $4219
    and #$08  ; Up
    cmp #$08
    bne JoypadDown  ; Button not pressed.
    lda $20
    cmp #31
    beq JoypadDown  ; Value saturated.
    inc $20

JoypadDown:
    lda $4219
    and #$04  ; Down
    cmp #$04
    bne JoypadLeft  ; Button not pressed.
    lda $20
    cmp #0
    beq JoypadLeft  ; Value saturated.
    dec $20

JoypadLeft:
    lda $4219
    and #$02  ; Left
    cmp #$02
    bne JoypadRight  ; Button not pressed.
    lda $22
    cmp #0
    beq JoypadRight  ; Value saturated.
    dec $22

JoypadRight:
    lda $4219
    and #$01
    cmp #$01  ; Right
    bne JoypadB  ; Button not pressed.
    lda $22
    cmp #31
    beq JoypadB  ; Value saturated.
    inc $22

JoypadB:
    lda $4219
    and #$80  ; B
    cmp #$80
    bne JoypadY  ; Button not pressed.
    lda $21
    cmp #31
    beq JoypadY  ; Value saturated.
    inc $21

JoypadY:
    lda $4219
    and #$40  ; Y
    cmp #$40
    bne JoypadDone  ; Button not pressed.
    lda $21
    cmp #0
    beq JoypadDone
    dec $21

JoypadDone:
    rts  


JoypadDebug:
    ; Load joypad registers into RAM for easier inspection.
    lda $4218
    sta $10
    lda $4219
    sta $11
    lda $421A
    sta $12
    lda $421B
    sta $13
    rts


SetBackgroundColor:
    ; $20 $21 $22 are R, G, B, as appropriate.
    ; The color format is 15-bit: [0bbbbbgg][gggrrrrr]
    ; We store the low-order byte first.
    lda $20   ; RED
    sta $00
    lda $22   ; BLUE
    asl
    asl
    sta $01
    lda $21   ; GREEN.
    ;  First store the high-order bits of green into $01.
    lsr
    lsr
    lsr
    ora $01
    pha  ; push 1
    ; Now store the low-order bits into $00.
    lda $21
    and #%00000111
    asl
    asl
    asl
    asl
    asl
    ora $00
    pha  ; push 2

    ; Now store the two values from the stack into the palette.
    pla  ; pull 2
    sta $2122
    pla  ; pull 1
    sta $2122
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

