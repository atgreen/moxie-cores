#!/bin/sh

# This test script is run by travis-ci.

# FIXME: don't assume we're in /root.  (ok for travis, however)

# Put the moxie build tools on the path...
export PATH=/opt/moxielogic/bin:$PATH

echo "************************************************************************"
echo Running icache testsuite
echo "************************************************************************"
cd /root/moxie-cores/bench/icache
make
sbcl --load test.lisp

echo "************************************************************************"
echo Building moxie core with iverilog
echo "************************************************************************"
cd /root/moxie-cores/soc/muskoka/iverilog
make clean
make


