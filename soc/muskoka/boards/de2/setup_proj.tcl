project_new muskoka -overwrite

# The original DE2: Cyclone II EP2C35 in the 672 pin package.
set_global_assignment -name FAMILY CycloneII
set_global_assignment -name DEVICE EP2C35F672C6
set_global_assignment -name TOP_LEVEL_ENTITY muskoka_de2

set_global_assignment -name RESERVE_ALL_UNUSED_PINS "AS INPUT TRI-STATED"
set_global_assignment -name RESERVE_ASDO_AFTER_CONFIGURATION "AS INPUT TRI-STATED"
set_global_assignment -name CYCLONEII_OPTIMIZATION_TECHNIQUE SPEED

# ---- Assign pins -------------------------------------

# The DE2 board has PIN_N2 as a 50MHz clock and PIN_D13 as a 27MHz
# clock.
set_location_assignment PIN_N2 -to clk_i

# Pushbutton KEY0 is reset.
set_location_assignment PIN_G26 -to key0_i

# RS-232
set_location_assignment PIN_B25 -to tx_o
set_location_assignment PIN_C25 -to rx_i

# Hex displays 0 to 3
set_location_assignment PIN_AF10 -to hex0_o[0]
set_location_assignment PIN_AB12 -to hex0_o[1]
set_location_assignment PIN_AC12 -to hex0_o[2]
set_location_assignment PIN_AD11 -to hex0_o[3]
set_location_assignment PIN_AE11 -to hex0_o[4]
set_location_assignment PIN_V14 -to hex0_o[5]
set_location_assignment PIN_V13 -to hex0_o[6]
set_location_assignment PIN_V20 -to hex1_o[0]
set_location_assignment PIN_V21 -to hex1_o[1]
set_location_assignment PIN_W21 -to hex1_o[2]
set_location_assignment PIN_Y22 -to hex1_o[3]
set_location_assignment PIN_AA24 -to hex1_o[4]
set_location_assignment PIN_AA23 -to hex1_o[5]
set_location_assignment PIN_AB24 -to hex1_o[6]
set_location_assignment PIN_AB23 -to hex2_o[0]
set_location_assignment PIN_V22 -to hex2_o[1]
set_location_assignment PIN_AC25 -to hex2_o[2]
set_location_assignment PIN_AC26 -to hex2_o[3]
set_location_assignment PIN_AB26 -to hex2_o[4]
set_location_assignment PIN_AB25 -to hex2_o[5]
set_location_assignment PIN_Y24 -to hex2_o[6]
set_location_assignment PIN_Y23 -to hex3_o[0]
set_location_assignment PIN_AA25 -to hex3_o[1]
set_location_assignment PIN_AA26 -to hex3_o[2]
set_location_assignment PIN_Y26 -to hex3_o[3]
set_location_assignment PIN_Y25 -to hex3_o[4]
set_location_assignment PIN_U22 -to hex3_o[5]
set_location_assignment PIN_W24 -to hex3_o[6]

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
