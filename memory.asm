; Memory layout:
; 0000-000F: scratch space for functions.
; 0010-0011: controller state of joypad #1.
; 0012-0013: controller state of joypad #2.
; 0014-0016: 24-bit counter of vblanks.
; 0017-0019: RGB color values to use for background color, from [0-31].
; 001A-001B: 16-bit pointer to next random byte.
; [gap]
; 0020-0021: (x, y) coordinates of player.
; 0022: player health.
; 0023: shot cooldown timer.
; 0024: next-shot state.
; [gap]
; 0030-003F: (x, y) velocities of each of the 8 possible shot states.
; 0040-009F: {sprite, x, y, x-velocity, y-velocity, unused} per player shot.
;            If sprite is 0, the shot is disabled.
; 00A0-015F: As above, for enemy shots.
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
.define playerHealth $22
.define shotCooldown $23
.define nextShotState $24
.define shotVelocityTable $30
.define playerShotArray $40
.define playerShotArrayLength 16
.define enemyShotArray $A0
.define enemyShotArrayLength 32
.define shotSize 6

.define numSprites 128
.define spriteTableStart $1000
.define spriteTable1Size $200
.define spriteTable2Start $1200
.define spriteTableSize $220
.define spriteTableScratchStart $1220



