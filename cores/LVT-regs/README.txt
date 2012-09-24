Live Value Table Memory Example Code

Copyright (c) 2010, Charles Eric LaForest, University of Toronto
Permission is granted for any use, and attribution would be nice. :)
Share and Enjoy!

If you found this useful, I'd love to hear about it: laforest@eecg.utoronto.ca
(gifts of hardware and books are also welcome ;)

This is some example code I lifted from my experiments. I've cleaned it up and
it should still work, but if not, it'll definitely give you the overall idea of
the Live Value Table (LVT) technique. 

The details are covered in the conference paper: "Efficient Multi-Ported
Memories for FPGAs", presented at the 18th annual ACM/SIGDA International
Symposium on Field Programmable Gate Arrays. You can also find it here:
http://www.eecg.toronto.edu/~laforest/

This code implements a 256x32 memory with two write ports and four read ports.
Everything is parametrized by using macros, except for the port configuration.
Changing that will require altering the files and connections between
components. It's a very regular pattern, so don't worry.

Files:

parameters.h
This file codifies the design parameters (memory width, depth, LVT entries,
memory bank names, etc...) into macros for legibility and portability. It must
be included at the beginning of all other design files.

MUX_WORD_2to1.v
A word-wide 2-to-1 multiplexer used to steer the read ports.
The selector input comes from the LVT.

MEM_1w1r.v
A synchronous memory with one write port and one read port.
This will usually be an exact match to a Block RAM.
You may have to constrain your synthesis to use the RAM type you want.

MEM_1w4r.v
Simply replicates four instances of the 1w1r modules using a common write port.

LVT_2w4r.v 
The Live Value Table proper. It's simply a synchronous memory, implemented in
the obvious way, with the same port configuration and the same depth as the
overall multi-ported memory. The end result will map to LUTs instead of Block
RAMs.  Note the width of this memory: it is log2(# of write ports). If you want
to implement some form of write contention management, this is where to do it.
Note the memory bank numbers at the writes.

MEM_2w4r.v

This module ties everything together: it instantiates the LVT, the two 1w4r
memory banks, and the multiplexers controlled by the LVT outputs, making
everything appear as a single 2w4r memory. There's only wiring in here, so you
should be able to trace out a block-level schematic easily.

