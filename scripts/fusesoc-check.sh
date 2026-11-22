#!/bin/sh

# fusesoc-check.sh
#
# Run every FuseSoC lint target, and every simulation with a program
# whose output is known, through FuseSoC itself, to check the core
# files.  Needs FuseSoC and Verilator on the PATH; the bench/moxibit
# programs, which `make' there builds, are used if they are there.

TOP=$(cd "$(dirname "$0")/.." && pwd)
WORK=$(mktemp -d)
trap 'rm -rf "$WORK"' EXIT
cd "$WORK"

pass=0
fail=0

# check NAME COMMAND... - run COMMAND, and pass if it succeeds.
check () {
  name=$1
  shift
  if "$@" > log 2>&1; then
    pass=$((pass + 1))
    echo "ok    $name"
  else
    fail=$((fail + 1))
    echo "FAIL  $name"
    grep -v '^INFO' log | tail -15
  fi
}

# output NAME TEXT COMMAND... - pass if COMMAND succeeds and prints TEXT.
output () {
  name=$1 text=$2
  shift 2
  check "$name" sh -c '"$@" > out 2>&1 && grep -q "$0" out' "$text" "$@"
}

F="fusesoc --cores-root $TOP"
C=moxielogic:moxie-cores

for core in mox125 moxibit mox125_board psram wishbone ram16bit uart3 \
	    nexys7seg hex_display mtimer statled gdbtarget altera_sdram \
	    lvt_regs bala; do
  check "lint $core" $F run --target=lint $C:$core
done

if [ -f "$TOP/bench/moxibit/bin/s011.x" ]; then
  output "sim moxibit-bench" CA1ECDB3 \
    $F run --target=sim $C:moxibit-bench --program="$TOP/bench/moxibit/bin/s011.x"
fi
output "sim mox125-board" "Hello from moxie!" \
  $F run --target=sim $C:mox125-board --program="$TOP/firmware/hello/hello.x"
check "sim bala, moxibit" \
  $F run --target=sim $C:bala --expect="Hello from moxie!"
check "sim bala, mox125" \
  $F run --flag=mox125 --target=sim $C:bala --expect="Hello from moxie!"
check "sim nexys3-soc, Zephyr" \
  $F run --target=sim $C:nexys3-soc \
    --srec="$TOP/soc/muskoka/boards/nexys3/prebuilt/zephyr-hello.srec" \
    --expect="Hello World" --max_clocks=100000000

echo "$pass passed, $fail failed"
test $fail -eq 0
