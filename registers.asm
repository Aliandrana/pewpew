; Definitions of commonly-used special memory addresses.
;
; These are commonly called "registers" in online documentation, even though
; that feels like a misnomer; these aren't necessarily hardware registers in
; the same sense as PC, A, X, Y, and so on.  Despite that, I call them
; "registers" too, since that's what everyone else calls them.
;
; I've often named these register definitions in the same way that they're
; named in Yoshi's venerable snes.txt document.  In some cases (where the
; mnemonic is too obscure) I've invented a different name.  In particular,
; I've changed "ADD" to "ADDR" to reduce possible confusion between "addresses"
; and "addition". The original name from Yoshi's doc is still listed in
; brackets, like [CGADD], for easy cross-referencing.
;
; I've also heavily borrowed from Yoshi's descriptions of what these registers
; do, though in many cases I've clarified / simplified the descriptions based
; on my own understanding, or simply reformatted them a bit.
;
; Here is a link to an online version of Yoshi's doc (v. 2.30):
; http://patpend.net/technical/snes/snes.txt
; Anomie's register doc is more up-to-date and might have better info:
; http://www.dforce3000.de/pub/doc/anomie_regs.txt

; $2100: Screen display initialization [INIDISP]
; Format: x000bbbb
; x: 0 = screen on, 1 = screen off, bbbb: Brightness ($0-$F)
.define INIDISP $2100

; $2101: OAM size [OBSEL]
; sssnnbbb    s: 000 =  8x8  or 16x16.
;                001 =  8x8  or 32x32.
;                010 =  8x8  or 64x64.
;                011 = 16x16 or 32x32.
;                100 = 16x16 or 64x64.
;                101 = 32x32 or 64x64.
;             n: Name selection (upper 4k word addr).
;             b: Base selection (8k word seg. addr).
.define OAMSIZE $2101

; $2102-$2103: OAM address register [OAMADDL/OAMADDH]
; aaaaaaaa r000000m    a: OAM address.
;                      r: OAM priority rotation.
;                      m: OAM address MSB.
.define OAMADDR $2102

; $2105: Screen mode register [BGMODE]
; abcdefff    a: BG4 tile size (0=8x8, 1=16x16).
;             b: BG3 tile size (0=8x8, 1=16x16).
;             c: BG2 tile size (0=8x8, 1=16x16).
;             d: BG1 tile size (0=8x8, 1=16x16).
;             e: Highest priority for BG3 in MODE 1.
;             f: MODE definition.
.define SCREENMODE $2105

; $2107-210A: BG1-4 tilemap registers [BGxSC]
; xxxxxxab    x: Base address (in VRAM, shifted left 11 bits).
;            ab: SC size (00=32x32 01=64x32 10=32x64 11=64x64)
.define BG1TILEMAP $2107
.define BG2TILEMAP $2108
.define BG3TILEMAP $2109
.define BG4TILEMAP $210A

; $210B: BG1 & BG2 VRAM location register [BG12NBA]
; $210C: BG3 & BG4 VRAM location register [BG34NBA]
; aaaabbbb    a: Base address for BG2 (or BG4).
;             b: Base address for BG1 (or BG3).
.define BG12NBA $210B
.define BG34NBA $210C

; BG1 horizontal scroll offset. [BG1HOFS]
; BG1 vertical scroll offset. [BG1VOFS]
; ... and similar registers for BG2-4.
; Write to all of these twice, as they want 2 bytes of data.
; mmmmmaaa aaaaaaaa    a: Horizontal offset.
;                      m: Only set with MODE 7.
.define BG1HOFS $210D
.define BG1VOFS $210E
.define BG2HOFS $210F
.define BG2VOFS $2110
.define BG3HOFS $2111
.define BG3VOFS $2112
.define BG4HOFS $2113
.define BG4VOFS $2114

; $2115: Video port control [VMAIN]
; i000abcd    i: 0 = Address increment after writing to $2118 or reading
;                    from $2139.
;                1 = Address increment after writing to $2119 or reading
;                    from $213A.
;            ab: Full graphic (see table below).
;            cd: SC increment (see table below).
;
; abcd Result
; 0100 Increment by 8 for 32 times (2-bit formation).
; 1000 Increment by 8 for 64 times (4-bit formation).
; 1100 Increment by 8 for 128 times (8-bit formation).
; 0000 Address increments 1x1.
; 0001 Address increments 32x32.
; 0010 Address increments 64x64.
; 0011 Address increments 128x128.
.define VMAIN $2115

; $2116-$2117: Video port address. 2 bytes. [VMADDL/VMADDH]
; Sets the initial address of a VRAM upload or download.
.define VMADDR $2116

; $2118-$2119: Video port data. 2 bytes. [VMDATAL/VMDATAH]
; According to bit 7 of VMAIN, the data can be stored as:
; Bit 7
;   0    Write to $2118 only.          Lower 8-bits written then
;                                      address is increased.
;   0    Write to $2119 then $2118.    Address increased when both
;                                      are written to (in order).
;   1    Write to $2119 only.          Upper 8-bits written, then
;                                      address is increased.
;   1    Write to $2118 then $2119.    Address increased when both
;                                      are written to (in order).
.define VMDATA $2118

; $2121: Color palette selection register [CGADD]
; Entry 0 corresponds to the SNES background color.
.define CGADDR $2121

; $2122: Color data register [CGDATA]
; The palette color format is 15-bit: [0bbbbbgg][gggrrrrr].
; You will typically write to this register twice in a row: first for the
; low-order byte (containing green and red) and then for the high-order byte
; (containing blue and green).
.define CGDATA $2122

; $212C: Main screen designation [TM]
; 000abcde    a: OBJ/OAM disable/enable.
;             b: Disable/enable BG4.
;             c: Disable/enable BG3.
;             d: Disable/enable BG2.
;             e: Disable/enable BG1.
.define MSENABLE $212C

; $4200: Counter enable [NMITIMEN]
; n-vh---j    n: NMI interrupt enable         v: vertical counter enable
;             h: horizontal counter enable    j: joypad enable
.define NMITIMEN $4200

; $420B: DMA enable [MDMAEN]
; Each bit that's set enables one channel: 76543210
.define DMAENABLE $420B

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

; $43x0: DMA control for channel x. [DMAPX]
; vh0cbaaa    v: 0 = CPU memory -> PPU.
;                1 = PPU -> CPU memory.
;             h: For HDMA only:
;                0 = Absolute addressing.
;                1 = Indirect addressing.
;             c: 0 = Auto address inc/decrement.
;                1 = Fixed address (for VRAM, etc.).
;             b: 0 = Automatic increment.
;                1 = Automatic decrement.
;             a: Transfer type:
;                000 = 1 address write twice: LH.
;                001 = 2 addresses: LH.
;                010 = 1 address write once.
;                011 = 2 addresses write twice: LLHH
;                100 = 4 addresses: LHLH
.define DMA0CTRL $4300
.define DMA1CTRL $4310
.define DMA2CTRL $4320
.define DMA3CTRL $4330
.define DMA4CTRL $4340
.define DMA5CTRL $4350
.define DMA6CTRL $4360
.define DMA7CTRL $4370

; $43x1: DMA destination for channel x. [BBADX]
; The upper byte is assumed to be $21, so the possible destinations are
; $2100-$21FF.
.define DMA0DST $4301
.define DMA1DST $4311
.define DMA2DST $4321
.define DMA3DST $4331
.define DMA4DST $4341
.define DMA5DST $4351
.define DMA6DST $4361
.define DMA7DST $4371

; $43x2-$43x3: DMA source address for channel x. 2 bytes. [AITXL/AITXH]
.define DMA0SRC $4302
.define DMA1SRC $4312
.define DMA2SRC $4322
.define DMA3SRC $4332
.define DMA4SRC $4342
.define DMA5SRC $4352
.define DMA6SRC $4362
.define DMA7SRC $4372

; $43x4: DMA source bank for channel x [AIBX]
.define DMA0SRCBANK $4304
.define DMA1SRCBANK $4314
.define DMA2SRCBANK $4324
.define DMA3SRCBANK $4334
.define DMA4SRCBANK $4344
.define DMA5SRCBANK $4354
.define DMA6SRCBANK $4364
.define DMA7SRCBANK $4374

; $43x5: DMA transfer size & HDMA address. 2 bytes. [DASXL/DASXH]
; When using DMA, $43x5 defines the # of bytes to be transferred via DMA
; itself. When using HDMA, $43x5 defines the data address ($43x5 = low byte,
; $43x6 = hi byte).
.define DMA0SIZE $4305
.define DMA1SIZE $4315
.define DMA2SIZE $4325
.define DMA3SIZE $4335
.define DMA4SIZE $4345
.define DMA5SIZE $4355
.define DMA6SIZE $4365
.define DMA7SIZE $4375
