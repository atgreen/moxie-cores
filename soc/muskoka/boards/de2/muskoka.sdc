# The DE2's 50 MHz oscillator; the PLL makes the 12.5 MHz the SoC runs at.
create_clock -name clk_i -period 20.000 [get_ports clk_i]
derive_pll_clocks
derive_clock_uncertainty

# The button, the serial port and the displays are all slow.
set_false_path -from [get_ports {key0_i rx_i}]
set_false_path -to [get_ports {tx_o hex0_o[*] hex1_o[*] hex2_o[*] hex3_o[*]}]
