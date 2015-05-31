Simple SNES shoot-'em-up game.

Run build.bat to build it. Assumes you have the wla assembler binaries and Neviksti's pcx2snes somewhere on your path.

The main files are:
pewpew.asm: main code file.
memory.asm: definitions of variable names (and memory layout) used by the game.
registers.asm: definitions & documentation of important SNES registers.
header.asm: definition of ROM layout & SNES header information for wla.
init.asm: boilerplate "initialize the hardware" macro.
