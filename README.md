moxie-cores - a repository of Moxie-compatible cores
=====================================================

[![CI](https://github.com/atgreen/moxie-cores/actions/workflows/ci.yml/badge.svg)](https://github.com/atgreen/moxie-cores/actions/workflows/ci.yml)

Cores
------

* MoxieLite - a bi-endian non-pipelined moxie implementation in VHDL.
* mox125 - a big-endian pipelined moxie implementation in Verilog,
  with exceptions, interrupts and a software-refilled MMU.  It runs
  Zephyr and Linux.
* moxibit - a tiny big-endian bit-serial moxie implementation in
  Verilog, in the style of SERV, with mox125's exceptions and bus but
  no MMU.  See cores/moxibit/README.md.

Wishbone peripherals live alongside them in `cores/`.

System-on-Chips
----------------

* marin - a MoxieLite-based SoC for the Spartan 6 based Nexys 3 and
  Cyclone II based DE2 eval boards.
* muskoka - a mox125-based SoC for the Cyclone II based DE2 and the
  Spartan 6 based Nexys 3 eval boards.
* bala - the smallest moxie system: either core, RAM and a serial
  port, for the iCEBreaker and ULX3S.  See soc/bala/README.md.

Simulators
-----------

* `bench/mox125` - the verilated mox125 core and its test suite.
* `bench/mox125-board` - the verilated core on a simulated board.
* `bench/moxibit` - the verilated moxibit core and its test suite.
* `bench/nexys3-soc` - the Nexys 3 Muskoka SoC under Verilator.
* `tools/mox125-iss` - an instruction-level simulator of mox125, for
  software too big to run on the verilated core.

FuseSoC
--------

The cores, peripherals and SoCs have FuseSoC core files, so this
repository can be a FuseSoC library
(`fusesoc library add moxie-cores https://github.com/atgreen/moxie-cores`,
or `--cores-root` on a checkout).  `fusesoc core list` shows them, and
`fusesoc core show <name>` their targets.  The core names are
`moxielogic:moxie-cores:<name>`; the main ones:

* `moxibit`, `mox125` - the cores, with lint (and synth) targets.  Both
  provide the virtual core `moxielogic:moxie-cores:cpu`, the module
  `moxie_cpu`, so an SoC can be built with either.
* `moxibit-bench`, `mox125-board`, `nexys3-soc` - simulations that run a
  program: `--program=`, or for the Nexys 3 SoC `--srec=` and
  `--expect=`.
* `bala` - sim, icebreaker and ulx3s targets.
* `muskoka-nexys3`, `muskoka-de2` - nexys3 (ISE 14.7) and de2 (Quartus
  13.0sp1) targets.

`scripts/fusesoc-check.sh` runs every lint and simulation target.

Testing
--------

`scripts/run-tests.sh` runs the test suites.  It needs Verilator 5,
[wrapilator](https://github.com/atgreen/wrapilator), SBCL,
[ocicl](https://github.com/ocicl/ocicl) and a moxie-elf toolchain.
`scripts/build-toolchain.sh` builds the toolchain, with the binutils
patch the mox125 extensions need.  `.github/workflows/ci.yml` shows
the whole setup.

See the HACKING file for notes on how to build, use and contribute to
moxie-cores.


Anthony Green
green@moxielogic.com
