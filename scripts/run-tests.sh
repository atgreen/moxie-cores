#!/bin/sh

# This test script is run by travis-ci.

# FIXME: don't assume we're in /root.  (ok for travis, however)

# Put the moxie build tools on the path...
export PATH=/opt/moxielogic/bin:$PATH

echo "************************************************************************"
echo Running verilated icache testsuite
echo "************************************************************************"
cd /root/moxie-cores/bench/icache
make clean
make
sbcl --load test.lisp

echo "************************************************************************"
echo Running verilated mox125 testsuite
echo "************************************************************************"
cd /root/moxie-cores/bench/mox125
make clean
make check

echo "************************************************************************"
echo Building moxie core with iverilog
echo "************************************************************************"
cd /root/moxie-cores/soc/muskoka/iverilog
make clean
make



