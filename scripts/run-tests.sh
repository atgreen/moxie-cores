#!/bin/sh

# run-tests.sh
#
# Run the verilated test suites and the instruction-level simulator's
# check, then make sure the C++ benches and the Muskoka SoC build.
# Needs moxie-elf, Verilator 5, wrapilator, sbcl and ocicl on the
# PATH; .github/workflows/ci.yml shows how to get them.

set -e

TOP=$(cd "$(dirname "$0")/.." && pwd)

banner () {
  echo "************************************************************************"
  echo "$@"
  echo "************************************************************************"
}

banner Running verilated icache testsuite
cd "$TOP/bench/icache"
ocicl install
make clean
make
sbcl --non-interactive --load test.lisp

banner Running verilated mox125 testsuite
cd "$TOP/bench/mox125"
ocicl install
make clean
make check

banner Running the bench programs on mox125-iss
cd "$TOP/tools/mox125-iss"
make clean
make check

banner Building the simulated board, the Nexys 3 SoC and Muskoka
make -C "$TOP/bench/mox125-board" clean all
make -C "$TOP/bench/nexys3-soc" clean all
make -C "$TOP/soc/muskoka/verilator" clean all
