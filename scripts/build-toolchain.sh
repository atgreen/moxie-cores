#!/bin/sh

# build-toolchain.sh PREFIX
#
# Copyright (c) 2026  Anthony Green
#
# Build the moxie-elf binutils, a C compiler and newlib's libc from
# release tarballs and install them under PREFIX.  This is the
# smallest toolchain that builds the bench test programs and the boot
# ROMs.  binutils and GCC get tools/binutils-moxie-extensions.patch and
# tools/gcc-moxie-extensions.patch: mox125's eret, wfi and cas.l, and
# the compact-code extension (docs/compact.md).  libgloss is left out:
# nothing here needs its system calls.

set -e

BINUTILS=binutils-2.47
GCC=gcc-15.3.0
NEWLIB=newlib-4.6.0.20260123

PREFIX=${1:?usage: $0 PREFIX}
TOP=$(cd "$(dirname "$0")/.." && pwd)
WORK=${WORK:-$(mktemp -d)}
JOBS=$(nproc)

mkdir -p "$PREFIX"
PREFIX=$(cd "$PREFIX" && pwd)
export PATH=$PREFIX/bin:$PATH

cd "$WORK"
[ -f $BINUTILS.tar.xz ] || curl -fsSLO https://ftp.gnu.org/gnu/binutils/$BINUTILS.tar.xz
[ -f $GCC.tar.xz ] || curl -fsSLO https://ftp.gnu.org/gnu/gcc/$GCC/$GCC.tar.xz
[ -f $NEWLIB.tar.gz ] || curl -fsSLO https://sourceware.org/pub/newlib/$NEWLIB.tar.gz
rm -rf $BINUTILS $GCC $NEWLIB build-*
tar xf $BINUTILS.tar.xz
tar xf $GCC.tar.xz
tar xf $NEWLIB.tar.gz
patch -d $BINUTILS -p1 < "$TOP/tools/binutils-moxie-extensions.patch"
patch -d $GCC -p1 < "$TOP/tools/gcc-moxie-extensions.patch"
(cd $GCC && ./contrib/download_prerequisites)

mkdir build-binutils
(cd build-binutils &&
  ../$BINUTILS/configure --target=moxie-elf --prefix="$PREFIX" \
      --disable-gdb --disable-gdbserver --disable-sim \
      --disable-werror --disable-nls &&
  make -j$JOBS && make install)

mkdir build-gcc
(cd build-gcc &&
  ../$GCC/configure --target=moxie-elf --prefix="$PREFIX" \
      --enable-languages=c --with-newlib --without-headers \
      --disable-libssp --disable-nls --disable-shared --disable-threads \
      --disable-libgomp --disable-libatomic --disable-libquadmath \
      --disable-decimal-float &&
  make -j$JOBS all-gcc all-target-libgcc &&
  make install-gcc install-target-libgcc)

mkdir build-newlib
(cd build-newlib &&
  ../$NEWLIB/configure --target=moxie-elf --prefix="$PREFIX" \
      --disable-newlib-supplied-syscalls --disable-nls &&
  make -j$JOBS all-target-newlib &&
  make install-target-newlib)
