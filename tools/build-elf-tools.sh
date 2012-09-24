#!/bin/sh

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

for dir in build/gcc-boot build/gcc build/src root/usr; do
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

  


