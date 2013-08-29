project_new muskoka -overwrite

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
set_location_assignment -to clk_i PIN_N2

# Assign pushbutton[0] to the reset signal.
set_location_assignment -to rst_i PIN_G26

set_location_assignment PIN_V13 -to hex0_[6]
set_location_assignment PIN_V14 -to hex0_[5]
set_location_assignment PIN_AE11 -to hex0_[4]
set_location_assignment PIN_AD11 -to hex0_[3]
set_location_assignment PIN_AC12 -to hex0_[2]
set_location_assignment PIN_AB12 -to hex0_[1]
set_location_assignment PIN_AF10 -to hex0_[0]
set_location_assignment PIN_AB24 -to hex1_[6]
set_location_assignment PIN_AA23 -to hex1_[5]
set_location_assignment PIN_AA24 -to hex1_[4]
set_location_assignment PIN_Y22 -to hex1_[3]
set_location_assignment PIN_W21 -to hex1_[2]
set_location_assignment PIN_V21 -to hex1_[1]
set_location_assignment PIN_V20 -to hex1_[0]
set_location_assignment PIN_Y24 -to hex2_[6]
set_location_assignment PIN_AB25 -to hex2_[5]
set_location_assignment PIN_AB26 -to hex2_[4]
set_location_assignment PIN_AC26 -to hex2_[3]
set_location_assignment PIN_AC25 -to hex2_[2]
set_location_assignment PIN_V22 -to hex2_[1]
set_location_assignment PIN_AB23 -to hex2_[0]
set_location_assignment PIN_Y23 -to hex3_[0]
set_location_assignment PIN_AA25 -to hex3_[1]
set_location_assignment PIN_AA26 -to hex3_[2]
set_location_assignment PIN_Y26 -to hex3_[3]
set_location_assignment PIN_Y25 -to hex3_[4]
set_location_assignment PIN_U22 -to hex3_[5]
set_location_assignment PIN_W24 -to hex3_[6]


# SRAM address lines - 18bits
set_location_assignment -to imem_address_o[0] PIN_AE4
set_location_assignment -to imem_address_o[1] PIN_AF4
set_location_assignment -to imem_address_o[2] PIN_AC5
set_location_assignment -to imem_address_o[3] PIN_AC6
set_location_assignment -to imem_address_o[4] PIN_AD4
set_location_assignment -to imem_address_o[5] PIN_AD5
set_location_assignment -to imem_address_o[6] PIN_AE5
set_location_assignment -to imem_address_o[7] PIN_AF5
set_location_assignment -to imem_address_o[8] PIN_AD6
set_location_assignment -to imem_address_o[9] PIN_AD7
set_location_assignment -to imem_address_o[10] PIN_V10
set_location_assignment -to imem_address_o[11] PIN_V9
set_location_assignment -to imem_address_o[12] PIN_AC7
set_location_assignment -to imem_address_o[13] PIN_W8
set_location_assignment -to imem_address_o[14] PIN_W10
set_location_assignment -to imem_address_o[15] PIN_Y10
set_location_assignment -to imem_address_o[16] PIN_AB8
set_location_assignment -to imem_address_o[17] PIN_AC8

# SRAM data lines - 16bits
set_location_assignment -to imem_data_i[0] PIN_AD8
set_location_assignment -to imem_data_i[1] PIN_AE6
set_location_assignment -to imem_data_i[2] PIN_AF6
set_location_assignment -to imem_data_i[3] PIN_AA9
set_location_assignment -to imem_data_i[4] PIN_AA10
set_location_assignment -to imem_data_i[5] PIN_AB10
set_location_assignment -to imem_data_i[6] PIN_AA11
set_location_assignment -to imem_data_i[7] PIN_Y11
set_location_assignment -to imem_data_i[8] PIN_AE7
set_location_assignment -to imem_data_i[9] PIN_AF7
set_location_assignment -to imem_data_i[10] PIN_AE8
set_location_assignment -to imem_data_i[11] PIN_AF8
set_location_assignment -to imem_data_i[12] PIN_W11
set_location_assignment -to imem_data_i[13] PIN_W12
set_location_assignment -to imem_data_i[14] PIN_AC9
set_location_assignment -to imem_data_i[15] PIN_AC10

# UART
set_location_assignment -to uart_txd_i PIN_B25 

# Create timing assignments
create_base_clock -fmax "50 MHz" -target PIN_N2 clk50_i

set_global_assignment -name VERILOG_FILE muskoka_de2.v
set_global_assignment -name VERILOG_FILE pll.v
set_global_assignment -name VERILOG_FILE ../../rtl/muskoka.v
set_global_assignment -name VERILOG_FILE ../../../../cores/hex_display/hex_display.v
set_global_assignment -name VERILOG_FILE ../../../../cores/hex_display/seg_7.v
set_global_assignment -name VERILOG_FILE ../../../../cores/mox125/cpu_fetch.v
set_global_assignment -name VERILOG_FILE ../../../../cores/mox125/cpu_ififo.v
set_global_assignment -name VERILOG_FILE ../../../../cores/mox125/cpu_decode.v
set_global_assignment -name VERILOG_FILE ../../../../cores/mox125/cpu_execute.v
set_global_assignment -name VERILOG_FILE ../../../../cores/mox125/cpu_write.v
set_global_assignment -name VERILOG_FILE ../../../../cores/mox125/cpu_registerfile.v
set_global_assignment -name VERILOG_FILE ../../../../cores/mox125/dcache.v
set_global_assignment -name VERILOG_FILE ../../../../cores/mox125/bootrom.v
set_global_assignment -name VERILOG_FILE ../../../../cores/mox125/microcode.v
set_global_assignment -name VERILOG_FILE ../../../../cores/mox125/moxie.v
set_global_assignment -name VERILOG_FILE ../../../../cores/wishbone/wb_intercon.v
set_global_assignment -name VERILOG_FILE ../../../../cores/altera_sdram/sdram.v
set_global_assignment -name VERILOG_FILE ../../../../cores/altera_sdram/sdram_controller.v
set_global_assignment -name VERILOG_FILE ../../../../cores/hex_display/hex_display.v
set_global_assignment -name VERILOG_FILE ../../../../cores/hex_display/seg_7.v
set_global_assignment -name VERILOG_FILE ../../../../cores/LVT-regs/LVT_2w4r.v
set_global_assignment -name VERILOG_FILE ../../../../cores/LVT-regs/MEM_1w1r.v
set_global_assignment -name VERILOG_FILE ../../../../cores/LVT-regs/MEM_1w4r.v
set_global_assignment -name VERILOG_FILE ../../../../cores/LVT-regs/MEM_2w4r.v
set_global_assignment -name VERILOG_FILE ../../../../cores/LVT-regs/MUX_WORD_2to1.v
set_global_assignment -name VERILOG_FILE ../../../../cores/uart3/uart_wb.v
set_global_assignment -name VERILOG_FILE ../../../../cores/uart3/uart.v
