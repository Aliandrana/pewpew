@echo off
wla-65816 -o pewpew.asm pewpew.o
wlalink -vr wlalink.cfg pewpew.smc
del *.o