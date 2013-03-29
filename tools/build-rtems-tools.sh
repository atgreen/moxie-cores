#!/bin/sh

# build-rtems-tools.sh
#
# Copyright (c) 2012  Anthony Green
# 
# The above named program is free software; you can redistribute it
# and/or modify it under the terms of the GNU General Public License
# version 2 as published by the Free Software Foundation.
# 
# The above named program is distributed in the hope that it will be
# useful, but WITHOUT ANY WARRANTY; without even the implied warranty
# of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this work; if not, write to the Free Software
# Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA
# 02110-1301, USA.

# A basic script to build the moxie-rtems toolchain.  It requires that the 
# GNU src & gcc trees be checked out in the current directory.

MAKEJOBS=4

if ! test -f src/src-release; then
  echo "ERROR: missing GNU src tree."
  exit 1
fi

if ! test -f gcc/gcc/version.h; then
  echo "ERROR: missing GNU gcc tree."
  exit 1
fi

for dir in buildrtems/gcc buildrtems/src buildrtems/gdb buildrtems/rtems root/usr; do
  if ! test -d $dir; then
    mkdir -p $dir;
  fi;
done;

# Put our tools on the PATH
PATH=$PREFIX/bin:$PATH
PREFIX=`(cd root/usr; pwd)`
WRAPPER=`(cd ../scripts/; pwd)`/moxie-rtems-gcc-wrapper

(cd buildrtems/src;
  ../../src/configure --target=moxie-rtems \
                      --with-sysroot=$PREFIX/moxie-rtems \
                      --disable-gdbtk \
                      --prefix=$PREFIX;
  make -j$MAKEJOBS all-binutils all-gas all-ld;
  make install-binutils install-gas install-ld)

(cd buildrtems/src;
  make -j$MAKEJOBS all-target-newlib all-target-libgloss \
      CC_FOR_TARGET=$WRAPPER;
  make install-target-newlib install-target-libgloss \
      CC_FOR_TARGET=$WRAPPER;)

(cd buildrtems/gcc; 
  ../../gcc/configure  --target=moxie-rtems \
                       --enable-threads=rtems \
                       --prefix=$PREFIX \
                       --enable-languages=c \
                       --disable-libssp \
                       --disable-libquadmath \
                       --with-gnu-as \
                       --with-gnu-ld \
                       --with-newlib;
  make -j$MAKEJOBS all;
  make install)

(cd buildrtems/src;
  make -j$MAKEJOBS all-sim all-gdb;
  make install-sim install-gdb)

(cd buildrtems/rtems;
  ../../rtems/configure --target=moxie-rtems \
                        --prefix=$PREFIX;
  make -j$MAKEJOBS;
  make install)


  
  



