# pins.tcl - the DE2 pins Muskoka uses, and the settings both of its builds share.
#
# Sourced by setup_proj.tcl, and by FuseSoC's Quartus project (see
# muskoka-de2.core).

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
