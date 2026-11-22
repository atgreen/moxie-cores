#!/bin/sh

# codesize.sh EMBENCH-DIR
#
# Copyright (c) 2026  Anthony Green
#
# Print the linked code size (.text) of each Embench-IoT benchmark
# (https://github.com/embench/embench-iot) at -Os: for moxie, with and
# without the compact-code extension (docs/compact.md), and, if
# riscv32-unknown-elf-gcc is on the PATH, for RV32I, RV32IC, RV32IM and
# RV32IMC.  Each benchmark is linked with libgcc and minilibc.c, from
# benchmark(), initialise_benchmark() and verify_benchmark(), with
# --gc-sections; RISC-V gets its linker relaxation.  The last lines
# give the totals and each column's geometric mean relative to RV32IC.

E=${1:?usage: $0 EMBENCH-DIR}
HERE=$(cd "$(dirname "$0")" && pwd)
T=$(mktemp -d)
trap 'rm -rf "$T"' EXIT

# Both targets use moxie's newlib headers, so that they compile the
# same code.
INC="-isystem $(dirname "$(command -v moxie-elf-gcc)")/../moxie-elf/include -I $E/support"
CFLAGS="-Os -std=gnu17 -fpermissive -w -DGLOBAL_SCALE_FACTOR=1 -DCPU_MHZ=1 -ffunction-sections -fdata-sections"
LDFLAGS="-nostdlib -Wl,--gc-sections -Wl,-e,benchmark -Wl,-u,verify_benchmark -Wl,-u,initialise_benchmark"

size () { # tool-prefix flags benchmark
  $1-gcc $CFLAGS -fno-builtin $2 -c "$HERE/minilibc.c" -o $T/libc.o &&
  $1-gcc $CFLAGS $2 $INC $LDFLAGS $E/src/$3/*.c $E/support/beebsc.c $T/libc.o \
      -lgcc -o $T/a.x 2>$T/err &&
  $1-objdump -h $T/a.x | awk '$2 ~ /^\.text/ {t += strtonum("0x" $3)} END {print t + 0}' ||
  { echo "$3 failed with $1 $2:" >&2; head -3 $T/err >&2; echo 0; }
}

set -- moxie-elf "" moxie-elf -mcompact moxie-elf "-mcompact -mgp"
NAMES="moxie compact gp"
if command -v riscv32-unknown-elf-gcc >/dev/null; then
  for a in rv32i rv32ic rv32im rv32imc; do
    set -- "$@" riscv32-unknown-elf "-march=$a -mabi=ilp32"
  done
  NAMES="$NAMES rv32i rv32ic rv32im rv32imc"
fi

{
  echo benchmark $NAMES
  for b in $(ls $E/src); do
    line=$b
    n=$#
    while [ $n -gt 0 ]; do
      line="$line $(size "$1" "$2" $b)"
      set -- "$@" "$1" "$2"; shift 2
      n=$((n - 2))
    done
    echo $line
  done
} | awk '
  NR == 1 { for (i = 2; i <= NF; i++) { name[i] = $i; if ($i == "rv32ic") ref = i }
	    printf "%-16s", "benchmark"; for (i = 2; i <= NF; i++) printf " %8s", $i; print ""; next }
  { printf "%-16s", $1
    for (i = 2; i <= NF; i++) { printf " %8d", $i; tot[i] += $i; v[NR, i] = $i }
    print ""; rows = NR; cols = NF }
  END {
    printf "%-16s", "total"; for (i = 2; i <= cols; i++) printf " %8d", tot[i]; print ""
    if (ref) {
      printf "%-16s", "/rv32ic, geomean"
      for (i = 2; i <= cols; i++) {
	s = 0; for (r = 2; r <= rows; r++) s += log(v[r, i] / v[r, ref])
	printf " %8.3f", exp(s / (rows - 1))
      }
      print ""
    }
  }'
