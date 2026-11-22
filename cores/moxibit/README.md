moxibit - a bit-serial moxie core
================================

moxibit is a small, non-pipelined moxie core that computes one bit per
clock, in the style of SERV (https://github.com/olofk/serv).  It is
big-endian, speaks mox125's 16-bit Wishbone bus, and so drops into the
same SoCs.

It implements the whole moxie instruction set with mox125's
exceptions: swi, brk, interrupts, undefined instructions and division
by zero, gsr/ssr for sr0-sr7, eret, wfi and cas.l.  It has no MMU and
no caches.


How it works
------------

Each instruction is a short microprogram, written as a table in
microcode.org: a sequence of 16-bit bus cycles and passes, where a
pass streams every operand through a one-bit datapath in 32 clocks,
LSB first.  ucoder.lisp turns the table into two ROMs, a microcode ROM
and a dispatch table indexed by opcode; run `make` here after changing
it.  The ROMs and the register file are block RAM on an FPGA.

There are two 32-bit shift registers.  ADR drives the bus address and
holds the PC between instructions; DBUF takes bus data into its low
half and drives bus data from its high half.  Everything else is in a
32-entry register file in one RAM, two bits wide, which gives a read
port to each of the datapath's two operands in turn: the 16 moxie
registers, sr1-sr7, and three slots the microcode uses as scratch.
The status word (interrupt enable and the five condition codes) is
flip-flops.

The comments at the top of moxibit.v and microcode.org go into detail.


Parameters
----------

- BOOT_ADDRESS (0x1000) - where execution starts after reset.
- MDU (1) - 0 drops the multiplier and divider: mul, div, udiv, mod
  and umod become undefined instructions, and
  firmware/moxibit/mdu-emulate.S emulates them.
- FAST_INC (0) - 1 adds 2 to ADR in parallel most of the time instead
  of in a pass, for about 12% fewer clocks overall (25-30% on code
  that calls a lot) at the cost of about 23 LUTs.

An instruction takes about 40 clocks for register arithmetic and
100-300 for memory access and calls; across the bench programs it is
112 on average, with the multiplier and divider weighing heavily.


Size
----

From `./synth.sh <serv checkout>` with Yosys 0.67, against SERV
synthesized the same way.  SERV has no multiplier or divider of its
own, so the fair comparison is moxibit with MDU = 0 against SERV with
its CSRs, which give it exceptions and interrupts.

    iCE40                 LUT4   FF   BRAM
    moxibit                 429  129     6
    moxibit, MDU = 0        383  121     6
    moxibit, FAST_INC = 1   452  130     6
    SERV                   212  164     1
    SERV with CSR          274  182     1

    Artix-7               LUT    FF   LUTRAM  BRAM
    moxibit                 303  131     8      1.5
    moxibit, MDU = 0        271  123     8      1.5
    SERV                   187  166     8      0
    SERV with CSR          256  184     9      0

Five of moxibit's six iCE40 block RAMs are its two ROMs.


FuseSoC
-------

moxibit.core describes the core to FuseSoC
(https://github.com/olofk/fusesoc), and
bench/moxibit/moxibit-bench.core the bench around it.  From anywhere,
with this repository as a core library:

    fusesoc --cores-root <moxie-cores> run --target=lint moxielogic:moxie-cores:moxibit
    fusesoc --cores-root <moxie-cores> run --target=synth moxielogic:moxie-cores:moxibit --MDU=0
    fusesoc --cores-root <moxie-cores> run --target=sim moxielogic:moxie-cores:moxibit-bench \
        --program=<moxie-cores>/bench/moxibit/bin/s011.x --wait=2

The sim target prints the program's result, as `make check` sees it;
`make` in bench/moxibit builds the programs.  MDU and FAST_INC can be
given to any of the targets.


Testing
-------

bench/moxibit has the tests.  `make check` there runs its own asm/*.S
programs and every bench/mox125 program that does not need an MMU, on
the core at 0, 1, 2 and 5 bus wait states, on the default build, and
on tools/mox125-iss; the asm/m*.S programs also run with MDU = 0.
`make lockstep` compares every instruction of every program with the
ISS: the registers and condition codes as well as the result.
