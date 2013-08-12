project_new marin -overwrite

# Assign family, device, and top-level file
set_global_assignment -name FAMILY CycloneII
set_global_assignment -name DEVICE EP2C35F672C6

# ---- Assign pins -------------------------------------

set_global_assignment -name RESERVE_ALL_UNUSED_PINS "AS INPUT TRI-STATED"
set_global_assignment -name STRATIX_DEVICE_IO_STANDARD LVCMOS
set_global_assignment -name RESERVE_ASDO_AFTER_CONFIGURATION "AS INPUT TRI-STATED"
set_global_assignment -name IGNORE_CLOCK_SETTINGS ON

set_global_assignment -name CYCLONE_OPTIMIZATION_TECHNIQUE SPEED

# The DE2 board has PIN_N2 as a 50MHz clock and PIN_D13 as a 27MHz
# clock.
set_location_assignment -to clk_external_i PIN_N2

# Assign pushbutton[0] to the reset signal.
set_location_assignment -to rst_i PIN_G26



# SRAM address lines - 18bits
#set_location_assignment -to imem_address_o[0] PIN_AE4
#set_location_assignment -to imem_address_o[1] PIN_AF4
#set_location_assignment -to imem_address_o[2] PIN_AC5
#set_location_assignment -to imem_address_o[3] PIN_AC6
#set_location_assignment -to imem_address_o[4] PIN_AD4
#set_location_assignment -to imem_address_o[5] PIN_AD5
#set_location_assignment -to imem_address_o[6] PIN_AE5
#set_location_assignment -to imem_address_o[7] PIN_AF5
#set_location_assignment -to imem_address_o[8] PIN_AD6
#set_location_assignment -to imem_address_o[9] PIN_AD7
#set_location_assignment -to imem_address_o[10] PIN_V10
#set_location_assignment -to imem_address_o[11] PIN_V9
#set_location_assignment -to imem_address_o[12] PIN_AC7
#set_location_assignment -to imem_address_o[13] PIN_W8
#set_location_assignment -to imem_address_o[14] PIN_W10
#set_location_assignment -to imem_address_o[15] PIN_Y10
#set_location_assignment -to imem_address_o[16] PIN_AB8
#set_location_assignment -to imem_address_o[17] PIN_AC8

# SRAM data lines - 16bits
#set_location_assignment -to imem_data_i[0] PIN_AD8
#set_location_assignment -to imem_data_i[1] PIN_AE6
#set_location_assignment -to imem_data_i[2] PIN_AF6
#set_location_assignment -to imem_data_i[3] PIN_AA9
#set_location_assignment -to imem_data_i[4] PIN_AA10
#set_location_assignment -to imem_data_i[5] PIN_AB10
#set_location_assignment -to imem_data_i[6] PIN_AA11
#set_location_assignment -to imem_data_i[7] PIN_Y11
#set_location_assignment -to imem_data_i[8] PIN_AE7
#set_location_assignment -to imem_data_i[9] PIN_AF7
#set_location_assignment -to imem_data_i[10] PIN_AE8
#set_location_assignment -to imem_data_i[11] PIN_AF8
#set_location_assignment -to imem_data_i[12] PIN_W11
#set_location_assignment -to imem_data_i[13] PIN_W12
#set_location_assignment -to imem_data_i[14] PIN_AC9
#set_location_assignment -to imem_data_i[15] PIN_AC10

# UART
set_location_assignment PIN_B25 -to tx_o
set_location_assignment PIN_C25 -to rx_i

# LEDs
set_location_assignment -to leds_o[0] PIN_AE23
set_location_assignment -to leds_o[1] PIN_AF23
set_location_assignment -to leds_o[2] PIN_AB21
set_location_assignment -to leds_o[3] PIN_AC22
set_location_assignment -to leds_o[4] PIN_AD22
set_location_assignment -to leds_o[5] PIN_AD23
set_location_assignment -to leds_o[6] PIN_AD21
set_location_assignment -to leds_o[7] PIN_AC21

# Hex display
set_location_assignment -to hex0_o[0] PIN_AF10
set_location_assignment -to hex0_o[1] PIN_AB12
set_location_assignment -to hex0_o[2] PIN_AC12
set_location_assignment -to hex0_o[3] PIN_AD11
set_location_assignment -to hex0_o[4] PIN_AE11
set_location_assignment -to hex0_o[5] PIN_V14
set_location_assignment -to hex0_o[6] PIN_V13
set_location_assignment -to hex1_o[0] PIN_V20
set_location_assignment -to hex1_o[1] PIN_V21
set_location_assignment -to hex1_o[2] PIN_W21
set_location_assignment -to hex1_o[3] PIN_Y22
set_location_assignment -to hex1_o[4] PIN_AA24
set_location_assignment -to hex1_o[5] PIN_AA23
set_location_assignment -to hex1_o[6] PIN_AB24
set_location_assignment -to hex2_o[0] PIN_AB23
set_location_assignment -to hex2_o[1] PIN_V22
set_location_assignment -to hex2_o[2] PIN_AC25
set_location_assignment -to hex2_o[3] PIN_AC26
set_location_assignment -to hex2_o[4] PIN_AB26
set_location_assignment -to hex2_o[5] PIN_AB25
set_location_assignment -to hex2_o[6] PIN_Y24
set_location_assignment -to hex3_o[0] PIN_Y23
set_location_assignment -to hex3_o[1] PIN_AA25
set_location_assignment -to hex3_o[2] PIN_AA26
set_location_assignment -to hex3_o[3] PIN_Y26
set_location_assignment -to hex3_o[4] PIN_Y25
set_location_assignment -to hex3_o[5] PIN_U22
set_location_assignment -to hex3_o[6] PIN_W24

set_location_assignment PIN_T6 -to DRAM_ADDR[0]
set_location_assignment PIN_V4 -to DRAM_ADDR[1]
set_location_assignment PIN_V3 -to DRAM_ADDR[2]
set_location_assignment PIN_W2 -to DRAM_ADDR[3]
set_location_assignment PIN_W1 -to DRAM_ADDR[4]
set_location_assignment PIN_U6 -to DRAM_ADDR[5]
set_location_assignment PIN_U7 -to DRAM_ADDR[6]
set_location_assignment PIN_U5 -to DRAM_ADDR[7]
set_location_assignment PIN_W4 -to DRAM_ADDR[8]
set_location_assignment PIN_W3 -to DRAM_ADDR[9]
set_location_assignment PIN_Y1 -to DRAM_ADDR[10]
set_location_assignment PIN_V5 -to DRAM_ADDR[11]
set_location_assignment PIN_AE2 -to DRAM_BA_0
set_location_assignment PIN_AE3 -to DRAM_BA_1
set_location_assignment PIN_AB3 -to DRAM_CAS_N
set_location_assignment PIN_AA6 -to DRAM_CKE
set_location_assignment PIN_AA7 -to DRAM_CLK
set_location_assignment PIN_AC3 -to DRAM_CS_N
set_location_assignment PIN_V6 -to DRAM_DQ[0]
set_location_assignment PIN_AA2 -to DRAM_DQ[1]
set_location_assignment PIN_AA1 -to DRAM_DQ[2]
set_location_assignment PIN_Y3 -to DRAM_DQ[3]
set_location_assignment PIN_Y4 -to DRAM_DQ[4]
set_location_assignment PIN_R8 -to DRAM_DQ[5]
set_location_assignment PIN_T8 -to DRAM_DQ[6]
set_location_assignment PIN_V7 -to DRAM_DQ[7]
set_location_assignment PIN_W6 -to DRAM_DQ[8]
set_location_assignment PIN_AB2 -to DRAM_DQ[9]
set_location_assignment PIN_AB1 -to DRAM_DQ[10]
set_location_assignment PIN_AA4 -to DRAM_DQ[11]
set_location_assignment PIN_AA3 -to DRAM_DQ[12]
set_location_assignment PIN_AC2 -to DRAM_DQ[13]
set_location_assignment PIN_AC1 -to DRAM_DQ[14]
set_location_assignment PIN_AA5 -to DRAM_DQ[15]
set_location_assignment PIN_AD2 -to DRAM_LDQM
set_location_assignment PIN_Y5 -to DRAM_UDQM
set_location_assignment PIN_AB4 -to DRAM_RAS_N
set_location_assignment PIN_AD3 -to DRAM_WE_N

# Create timing assignments
create_base_clock -fmax "50 MHz" -target PIN_N2 clk50_i

set_global_assignment -name VERILOG_FILE ../../rtl/marin.v
set_global_assignment -name VERILOG_FILE ../../rtl/bootrom16.v
set_global_assignment -name VERILOG_FILE ../../../../cores/wishbone/wb_intercon.v
set_global_assignment -name VHDL_FILE ../../../../cores/MoxieLite/moxielite_alu2.vhd
set_global_assignment -name VHDL_FILE ../../../../cores/MoxieLite/moxielite_alu.vhd
set_global_assignment -name VHDL_FILE ../../../../cores/MoxieLite/moxielite_decode.vhd
set_global_assignment -name VHDL_FILE ../../../../cores/MoxieLite/moxielite_divider.vhd
set_global_assignment -name VHDL_FILE ../../../../cores/MoxieLite/moxielite_lshift.vhd
set_global_assignment -name VHDL_FILE ../../../../cores/MoxieLite/moxielite_multiplier.vhd
set_global_assignment -name VHDL_FILE ../../../../cores/MoxieLite/moxielite_package.vhd
set_global_assignment -name VHDL_FILE ../../../../cores/MoxieLite/moxielite_rshift.vhd
set_global_assignment -name VHDL_FILE ../../../../cores/MoxieLite/moxielite.vhd
set_global_assignment -name VHDL_FILE ../../../../cores/MoxieLite/sim_ram.vhd
set_global_assignment -name VERILOG_FILE ../../../../cores/MoxieLite/moxielite_wb.v
set_global_assignment -name VERILOG_FILE ../../../../cores/wishbone/wb_intercon.v
set_global_assignment -name VERILOG_FILE ../../../../cores/wishbone/wb_watchdog.v
set_global_assignment -name VERILOG_FILE ../../../../cores/ram16bit/ram16bit_wb.v
set_global_assignment -name VERILOG_FILE ../../../../cores/uart3/uart.v
set_global_assignment -name VERILOG_FILE ../../../../cores/uart3/uart_wb.v
set_global_assignment -name VERILOG_FILE ../../../../cores/mpic/mpic.v
set_global_assignment -name VERILOG_FILE ../../../../cores/mtimer/mtimer.v
set_global_assignment -name VERILOG_FILE ../../../../cores/hex_display/hex_display.v
set_global_assignment -name VERILOG_FILE ../../../../cores/hex_display/hex_display_wb.v
set_global_assignment -name VERILOG_FILE ../../../../cores/hex_display/seg_7.v
set_global_assignment -name VERILOG_FILE ../../../../cores/altera_sdram/sdram_controller.v


project_close
