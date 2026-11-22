Bala - the smallest moxie system
================================

Bala is a moxie core, RAM preloaded with a program, and the
mox125_board peripherals, in the spirit of SERV's servant.  It is for
trying a core on a new board, and for running small programs.

    0x00000000  RAM, 8 KiB by default (RAM_BYTES), repeating; the core
                starts at 0x1000
    0xF0000000  console, interrupt controller, timer, display, and a
                16550 UART on the same serial line

That is the board bench/mox125-board, tools/mox125-iss and the Nexys 3
SoC have, so software for them runs here if it fits.  The RAM is
loaded from hello.vh (firmware/hello), which prints a line on the
serial port; give the INIT parameter another file of 16-bit words to
run something else.

The core is either moxibit or mox125: Bala instantiates moxie_cpu,
which both provide.  moxibit is the default; `--flag=mox125` picks the
other.


With FuseSoC
------------

From the top of moxie-cores:

    fusesoc --cores-root . run --target=sim moxielogic:moxie-cores:bala --expect="moxie!"
    fusesoc --cores-root . run --flag=mox125 --target=sim moxielogic:moxie-cores:bala
    fusesoc --cores-root . run --target=icebreaker moxielogic:moxie-cores:bala
    fusesoc --cores-root . run --target=ulx3s moxielogic:moxie-cores:bala

- sim runs the SoC under Verilator and prints what it sends on its
  serial port (bench/bala_tb.cc).  With --expect it stops, and
  succeeds, once that text has arrived, and fails if it never does.
- icebreaker builds a bitstream for the 1BitSquared iCEBreaker (iCE40
  UP5K, 12 MHz) with Yosys, nextpnr and icepack; moxibit only, since
  mox125 does not fit.  With moxibit it takes 1270 of the 5280 logic
  cells and 22 of the 30 block RAMs, and closes timing at 22.7 MHz.
- ulx3s builds one for the Radiona ULX3S with an LFE5U-85F (25 MHz)
  with Yosys, nextpnr and ecppack, with either core; mox125 closes
  timing at 30.8 MHz.

The serial port runs at 115200 8N1 on both boards.  Neither bitstream
has been tried on a board yet.

MDU and FAST_INC are passed to moxibit; see cores/moxibit/README.md.
