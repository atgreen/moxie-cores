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

Simulators
-----------

* `bench/mox125` - the verilated mox125 core and its test suite.
* `bench/mox125-board` - the verilated core on a simulated board.
* `bench/moxibit` - the verilated moxibit core and its test suite.
* `bench/nexys3-soc` - the Nexys 3 Muskoka SoC under Verilator.
* `tools/mox125-iss` - an instruction-level simulator of mox125, for
  software too big to run on the verilated core.

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
