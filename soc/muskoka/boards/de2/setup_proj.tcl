project_new muskoka -overwrite

# The original DE2: Cyclone II EP2C35 in the 672 pin package.
set_global_assignment -name FAMILY CycloneII
set_global_assignment -name DEVICE EP2C35F672C6
set_global_assignment -name TOP_LEVEL_ENTITY muskoka_de2

source pins.tcl

# ---- Timing ------------------------------------------

set_global_assignment -name SDC_FILE muskoka.sdc

# ---- Sources -----------------------------------------

# For `include "defines.h" and `include "config.v"
set_global_assignment -name SEARCH_PATH ../../../../cores/mox125
set_global_assignment -name SEARCH_PATH .

set_global_assignment -name VERILOG_FILE muskoka_de2.v
set_global_assignment -name VERILOG_FILE pll.v
set_global_assignment -name VERILOG_FILE ../../rtl/muskoka.v
set_global_assignment -name VERILOG_FILE ../../rtl/bootrom16.v
set_global_assignment -name VERILOG_FILE ../../../../cores/mox125/moxie.v
set_global_assignment -name VERILOG_FILE ../../../../cores/mox125/cpu_fetch.v
set_global_assignment -name VERILOG_FILE ../../../../cores/mox125/cpu_decode.v
set_global_assignment -name VERILOG_FILE ../../../../cores/mox125/cpu_execute.v
set_global_assignment -name VERILOG_FILE ../../../../cores/mox125/cpu_registerfile.v
set_global_assignment -name VERILOG_FILE ../../../../cores/mox125/cpu_write.v
set_global_assignment -name VERILOG_FILE ../../../../cores/mox125/dcache.v
set_global_assignment -name VERILOG_FILE ../../../../cores/mox125/icache.v
set_global_assignment -name VERILOG_FILE ../../../../cores/mox125/microcode.v
set_global_assignment -name VERILOG_FILE ../../../../cores/mox125/tlb.v
set_global_assignment -name VERILOG_FILE ../../../../cores/wishbone/wb_intercon.v
set_global_assignment -name VERILOG_FILE ../../../../cores/ram16bit/ram16bit_wb.v
set_global_assignment -name VERILOG_FILE ../../../../cores/uart3/uart.v
set_global_assignment -name VERILOG_FILE ../../../../cores/uart3/uart_wb.v
set_global_assignment -name VERILOG_FILE ../../../../cores/hex_display/hex_display.v
set_global_assignment -name VERILOG_FILE ../../../../cores/hex_display/hex_display_wb.v
set_global_assignment -name VERILOG_FILE ../../../../cores/hex_display/seg_7.v

project_close
