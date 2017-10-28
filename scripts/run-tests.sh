#!/bin/sh

# Put the moxie build tools on the path...
export PATH=/opt/moxielogic/bin:$PATH

cd /root/moxie-cores/bench
make
sbcl --load test.lisp

