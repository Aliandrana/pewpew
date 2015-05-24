@echo off
pcx2snes sprites.pcx -osprites.asm -b4 -nSprite -d
pcx2snes tiles.pcx -otiles.asm -b2 -nTile -d
wla-65816 -o pewpew.asm pewpew.o
wlalink -vr wlalink.cfg pewpew.smc
del *.o