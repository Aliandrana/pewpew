@echo off
pcx2snes tiles.pcx -otiles.asm -b2 -nTile -d
wla-65816 -o pewpew.asm pewpew.o
wlalink -vr wlalink.cfg pewpew.smc
del *.o