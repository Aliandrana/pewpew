@echo off
pcx2snes -s32 -c16 -o16 -n sprites32
pcx2snes -s16 -c4 -o4 -n tiles
wla-65816 -o pewpew.asm pewpew.o
wlalink -vr wlalink.cfg pewpew.smc
del *.o