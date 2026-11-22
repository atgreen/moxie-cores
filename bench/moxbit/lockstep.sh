#!/bin/sh
# lockstep.sh program.x [wait]
#
# Run a program on the verilated core and on tools/mox125-iss, each
# printing the address, opcode, registers and condition codes before
# every instruction, and report the first instruction where they
# differ.  Needs tb-trace, from `make tb-trace'.  The core sees memory
# as the ISS does (tb -d).
#
# A program that never exits is stopped by each side's limit, at
# different places; then the two have to agree as far as the shorter.

here=$(dirname "$0")
x=${1:?usage: $0 program.x [wait]}
tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT

"$here/../../tools/mox125-iss/mox125-iss" -r -T -n 2000000 "$x" 2> "$tmp/iss" > /dev/null
"$here/tb-trace" -d -w "${2:-0}" -n 20000000 "$x" 2> /dev/null > "$tmp/out"
rtl_exited=$?
grep -E '^[0-9a-f]{8} ' "$tmp/out" > "$tmp/rtl"
ni=$(wc -l < "$tmp/iss")
nr=$(wc -l < "$tmp/rtl")

if [ $rtl_exited -ne 0 ] && [ "$nr" -lt "$ni" ]; then
  head -n "$nr" "$tmp/iss" > "$tmp/iss.short"
  mv "$tmp/iss.short" "$tmp/iss"
  note=" (neither exits)"
elif [ $rtl_exited -ne 0 ] && [ "$ni" -ge 2000000 ]; then
  head -n "$ni" "$tmp/rtl" > "$tmp/rtl.short"
  mv "$tmp/rtl.short" "$tmp/rtl"
  note=" (neither exits)"
fi

if cmp -s "$tmp/iss" "$tmp/rtl"; then
  echo "$(wc -l < "$tmp/rtl") instructions agree$note"
  exit 0
fi
n=$(cmp "$tmp/iss" "$tmp/rtl" 2>/dev/null | sed -n 's/.* line \([0-9]*\).*/\1/p')
if [ -z "$n" ]; then
  echo "one side stops early: ISS $(wc -l < "$tmp/iss"), RTL $(wc -l < "$tmp/rtl") instructions"
  exit 1
fi
echo "instruction $n differs (address opcode fp sp r0-r13 cc):"
[ "$n" -gt 1 ] && { echo "both:"; sed -n "$((n - 1))p" "$tmp/iss"; }
echo "ISS:"; sed -n "${n}p" "$tmp/iss"
echo "RTL:"; sed -n "${n}p" "$tmp/rtl"
exit 1
