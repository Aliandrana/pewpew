; Definitions of commonly-used special memory addresses.
;
; These are commonly called "registers" in online documentation, even though
; that feels like a misnomer; these aren't necessarily hardware registers in
; the same sense as PC, A, X, Y, and so on.  Despite that, I call them
; "registers" too, since that's what everyone else calls them.
;
; Where possible, I have named these register definitions in the same way that
; they're named in Yoshi's venerable snes.txt document.

; $2100: Screen display register [INIDISP]
; Format: x000bbbb 
; x: 0 = screen on, 1 = screen off, bbbb: Brightness ($0-$F)
.define INIDISP $2100

; $2121: Color palette selection register [CGADD]
; Entry 0 corresponds to the SNES background color.
.define CGADD $2121

; $2122: Color data register [CGDATA]
; The palette color format is 15-bit: [0bbbbbgg][gggrrrrr].
; You will typically write to this register twice in a row: first for the
; low-order byte (containing green and red) and then for the high-order byte
; (containing blue and green).
.define CGDATA $2122

; $4200: Counter enable [NMITIMEN]
; n-vh---j   n: NMI interrupt enable         v: vertical counter enable
;            h: horizontal counter enable    j: joypad enable
.define NMITIMEN $4200

; $4218: Joypad #1 status [JOY1L]
; Format: AXLR0000
.define JOY1L $4218

; $4219: Joypad #1 status [JOY1H]
; Format: BYsSudlr (s=select, S=start, udlr = joypad)
.define JOY1H $4219

; $421A: Joypad #2 status [JOY2L]
; Format: AXLR0000
.define JOY2L $421A

; $421B: Joypad #2 status [JOY2H]
; Format: BYsSudlr (s=select, S=start, udlr = joypad)
.define JOY2H $421B
