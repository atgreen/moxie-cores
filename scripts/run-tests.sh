#!/bin/sh

# Put the moxie build tools on the path...
export PATH=/opt/moxielogic/bin:$PATH

echo "************************************************************************"
echo Running icache testsuite
echo "************************************************************************"
cd /root/moxie-cores/bench
make
sbcl --load test.lisp

echo "************************************************************************"
echo Building moxie core with iverilog
echo "************************************************************************"
cd /root/moxie-cores/soc/muskoka/iverilog
make clean
make


