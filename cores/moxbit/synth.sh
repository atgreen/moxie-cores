#!/bin/sh
# synth.sh [serv-checkout]
#
# Synthesize moxbit with Yosys for iCE40 and Artix-7, and print what it
# takes.  Given a checkout of SERV (https://github.com/olofk/serv), do
# the same for it, the same way, to compare against.

here=$(cd "$(dirname "$0")" && pwd)
serv=$1
out=$(mktemp -d)
trap 'rm -rf "$out"' EXIT

synth () { # name family top parameter files...
  name=$1 fam=$2 top=$3 param=$4; shift 4
  case $fam in
    ice40) cmd="synth_ice40 -top $top" ;;
    xc7)   cmd="synth_xilinx -family xc7 -flatten -top $top" ;;
  esac
  st=$out/$name.$fam
  if ! yosys -q -p "read_verilog -I$here $*; chparam $param $top; $cmd; tee -o $st stat" > /dev/null 2>&1; then
    echo "$name $fam: failed"
    return
  fi
  if [ $fam = ice40 ]; then
    printf '%-20s %-6s LUT4 %4s  FF %4s  BRAM %s\n' $name $fam \
      $(awk '$2=="SB_LUT4"{print $1}' $st) \
      $(awk '$2~/^SB_DFF/{s+=$1} END{print s+0}' $st) \
      $(awk '$2=="SB_RAM40_4K"{n=$1} END{print n+0}' $st)
  else
    printf '%-20s %-6s LUT  %4s  FF %4s  LUTRAM %s  BRAM %s\n' $name $fam \
      $(awk '$2~/^LUT[1-6]$/{s+=$1} END{print s+0}' $st) \
      $(awk '$2~/^FD/{s+=$1} END{print s+0}' $st) \
      $(awk '$2~/^RAM(32|64|128|256)/{s+=$1} END{print s+0}' $st) \
      $(awk '$2=="RAMB18E1"{s+=$1} $2=="RAMB36E1"{s+=2*$1} END{print s/2}' $st)
  fi
}

moxbit="$here/moxbit.v $here/moxbit_rf.v $here/moxbit_ucode.v"
for fam in ice40 xc7; do
  synth moxbit       $fam moxbit "-set MDU 1" $moxbit
  synth moxbit-mdu0  $fam moxbit "-set MDU 0" $moxbit
  synth moxbit-fast  $fam moxbit "-set MDU 1 -set FAST_INC 1" $moxbit
  if [ -n "$serv" ]; then
    files=$(ls "$serv"/rtl/serv_*.v | grep -v serv_debug)
    synth serv         $fam serv_rf_top "-set WITH_CSR 0" $files
    synth serv-csr     $fam serv_rf_top "-set WITH_CSR 1" $files
  fi
done
