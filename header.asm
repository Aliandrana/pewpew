; LoRom header.

.MEMORYMAP
  SLOTSIZE $8000                ; The slot is $8000 bytes in size.
  DEFAULTSLOT 0                 ; The SNES only has 1 slot.
  SLOT 0 $8000                  ; Define's Slot 0's starting address.
.ENDME

.ROMBANKSIZE $8000              ; Every ROM bank is 32 KB in size.
.ROMBANKS 8                     ; 8 ROM banks = 2 Mb (256 KB).

.SNESHEADER
  ID "SNES"
  
  NAME "PEW PEW              "  ; Program title. Should be 21 bytes long;
  ;    "123456789012345678901"  ; use spaces for unused bytes of the name.

  SLOWROM
  LOROM

  CARTRIDGETYPE $00             ; $00 = ROM only.
  ROMSIZE $08                   ; $08 = 2 Mbits.
  SRAMSIZE $00                  ; No SRAM.
  COUNTRY $01                   ; $01 = U.S.; $00 = Japan.
  LICENSEECODE $00
  VERSION $00                   ; $00 = 1.00, $01 = 1.01, etc.
.ENDSNES

.SNESNATIVEVECTOR               ; Native Mode interrupt vector table.
  COP EmptyHandler
  BRK EmptyHandler
  ABORT EmptyHandler
  NMI VBlankHandler
  IRQ EmptyHandler
.ENDNATIVEVECTOR

.SNESEMUVECTOR                  ; Emulation Mode interrupt vector table.
  COP EmptyHandler
  ABORT EmptyHandler
  NMI EmptyHandler
  RESET Start
  IRQBRK EmptyHandler
.ENDEMUVECTOR

; Defines the ROM bank and the slot it is inserted in memory.
; .ORG 0 is really $8000, because the slot starts at $8000.
.BANK 0 SLOT 0
.ORG 0
.SECTION "EmptyVectors" SEMIFREE

EmptyHandler:
  rti

.ENDS

.EMPTYFILL $00