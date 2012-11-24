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
set_location_assignment -to clk_i PIN_N2

# Assign pushbutton[0] to the reset signal.
set_location_assignment -to rst_i PIN_G26

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
set_global_assignment -name VERILOG_FILE ../../../../cores/statled/rtl/statled.v
set_global_assignment -name VERILOG_FILE ../../../../cores/gdbtarget/gdbtarget.v
set_global_assignment -name VERILOG_FILE ../../../../cores/uart/uart.v
set_global_assignment -name VERILOG_FILE ../../../../cores/uart/uart_wb.v
set_global_assignment -name VERILOG_FILE ../../../../cores/nexys7seg/nexys7seg.v

project_close
