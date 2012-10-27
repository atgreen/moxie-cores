#!/bin/sh

# build-elf-tools.sh
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

# A basic script to build the moxie-elf toolchain.  It requires that the 
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

for dir in build/gcc-boot build/gcc build/src build/gdb root/usr; do
  if ! test -d $dir; then
    mkdir -p $dir;
  fi;
done;

PREFIX=`(cd root/usr; pwd)`

(cd build/src;
  ../../src/configure --target=moxie-elf \
                      --disable-gdbtk \
                      --prefix=$PREFIX;
  make -j$MAKEJOBS all-binutils all-gas all-ld;
  make install-binutils install-gas install-ld)

(cd build/gcc-boot; 
  ../../gcc/configure  --target=moxie-elf \
                       --prefix=$PREFIX \
                       --enable-languages=c \
                       --disable-libssp \
                       --disable-libquadmath \
                       --without-newlib;
  make -j$MAKEJOBS all;
  make install)

# Put our new tools on the PATH
PATH=$PREFIX/root/usr/bin:$PATH

(cd build/src;
  make -j$MAKEJOBS all-target-newlib all-target-libgloss \
      CC_FOR_TARGET=moxie-elf-gcc;
  make install-target-newlib install-target-libgloss \
      CC_FOR_TARGET=moxie-elf-gcc;)

(cd build/gcc;
  ../../gcc/configure  --target=moxie-elf \
                       --prefix=`(cd ../../root/usr; pwd)` \
                       --enable-languages=c,c++ \
                       --disable-libssp \
                       --disable-libquadmath \
                       --with-newlib \
                       --with-headers=$PREFIX/moxie-elf/include;
  make -j$MAKEJOBS all;
  make install)

(cd build/src;
  make -j$MAKEJOBS all-sim all-gdb;
  make install-sim install-gdb)

  
  



