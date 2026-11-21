// muskoka_nexys3.v - The Muskoka SoC on the Digilent Nexys 3.
//
// Copyright (c) 2026  Anthony Green
//
// The above named program is free software; you can redistribute it
// and/or modify it under the terms of the GNU General Public License
// version 2 as published by the Free Software Foundation.
//
// The above named program is distributed in the hope that it will be
// useful, but WITHOUT ANY WARRANTY; without even the implied warranty
// of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this work; if not, write to the Free Software
// Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA
// 02110-1301, USA.

// The pins and the clock; nexys3_soc.v is everything else.
//
// The board's 100 MHz oscillator is multiplied up to 800 MHz and
// divided by CLOCK_DIVIDE.  The default of 64 gives 12.5 MHz, which
// is deliberately slow: the core has not been through timing closure
// on this part.  To go faster, change CLOCK_DIVIDE and UART_DIVISOR
// together (and the clock rate in whatever software cares) and see
// what the timing report says.
//
// The centre pushbutton is reset.  The display shows what software
// last wrote to the display register.  LED 7 is lit while the clock
// generator is locked, LED 0 while the core is held in reset, and
// LEDs 1 and 2 flicker with the serial port.

module muskoka_nexys3 #(parameter CLOCK_DIVIDE = 64,
			// (800 MHz / CLOCK_DIVIDE) / 115200, less one
			parameter UART_DIVISOR = 108
			)(/*AUTOARG*/
   // Outputs
   tx_o, leds_o, seg_o, an_o, mem_adr_o, mem_cen_o, mem_oen_o,
   mem_wen_o, mem_ubn_o, mem_lbn_o, mem_adv_o, mem_clk_o, mem_cre_o,
   flash_cen_o, qspi_csn_o,
   // Inouts
   mem_dat_io,
   // Inputs
   clk_i, btn_reset_i, rx_i
   );

  input         clk_i;        // 100 MHz
  input         btn_reset_i;  // high while pressed

  input         rx_i;
  output        tx_o;

  output [7:0]  leds_o;
  output [7:0]  seg_o;
  output [3:0]  an_o;

  // Cellular RAM
  output [25:0] mem_adr_o;
  inout [15:0]  mem_dat_io;
  output        mem_cen_o;
  output        mem_oen_o;
  output        mem_wen_o;
  output        mem_ubn_o;
  output        mem_lbn_o;
  output        mem_adv_o;
  output        mem_clk_o;
  output        mem_cre_o;

  // The flash memories that share the RAM's pins
  output        flash_cen_o;
  output        qspi_csn_o;

  // --- Clock ----------------------------------------------------
  wire          clk_in, clk_fb, clk_pll, clk_cpu, locked;

  IBUFG clk_buf (.I (clk_i), .O (clk_in));

  PLL_BASE #(.CLKIN_PERIOD (10.0),
	     .CLKFBOUT_MULT (8),
	     .DIVCLK_DIVIDE (1),
	     .CLKOUT0_DIVIDE (CLOCK_DIVIDE),
	     .COMPENSATION ("INTERNAL")) pll (.CLKIN (clk_in),
					      .CLKFBIN (clk_fb),
					      .CLKFBOUT (clk_fb),
					      .CLKOUT0 (clk_pll),
					      .CLKOUT1 (),
					      .CLKOUT2 (),
					      .CLKOUT3 (),
					      .CLKOUT4 (),
					      .CLKOUT5 (),
					      .LOCKED (locked),
					      .RST (1'b0));

  BUFG cpu_buf (.I (clk_pll), .O (clk_cpu));

  // Hold reset until the clock is steady, and let go of it in step
  // with the clock.
  reg [3:0]     rst_sync = 4'b1111;
  wire          rst = rst_sync[3];

  always @(posedge clk_cpu)
    rst_sync <= {rst_sync[2:0], btn_reset_i | !locked};

  // --- The SoC --------------------------------------------------
  wire [22:0]   mem_adr;
  wire [15:0]   mem_dat_out;
  wire          mem_dat_oe;
  wire [15:0]   display;

  nexys3_soc #(.UART_DIVISOR (UART_DIVISOR)) soc (.rst_i (rst),
						  .clk_i (clk_cpu),
						  .rx_i (rx_i),
						  .tx_o (tx_o),
						  .display_o (display),
						  .mem_adr_o (mem_adr),
						  .mem_cen_o (mem_cen_o),
						  .mem_oen_o (mem_oen_o),
						  .mem_wen_o (mem_wen_o),
						  .mem_ubn_o (mem_ubn_o),
						  .mem_lbn_o (mem_lbn_o),
						  .mem_dat_i (mem_dat_io),
						  .mem_dat_o (mem_dat_out),
						  .mem_dat_oe_o (mem_dat_oe));

  // The RAM is addressed in 16-bit words; its address 0 is the lowest
  // address pin on the board.  It has 23 address bits; the pins above
  // those only go to the flash.
  assign mem_adr_o = {3'b000, mem_adr};
  assign mem_dat_io = mem_dat_oe ? mem_dat_out : 16'bz;

  // Asynchronous operation: no clock, addresses always valid, and
  // never the configuration register.
  assign mem_clk_o = 1'b0;
  assign mem_adv_o = 1'b0;
  assign mem_cre_o = 1'b0;

  assign flash_cen_o = 1'b1;
  assign qspi_csn_o = 1'b1;

  // --- Lights ---------------------------------------------------
  nexys7seg digits (.clk (clk_cpu),
		    .word (display),
		    .seg (seg_o),
		    .an (an_o));

  assign leds_o = {locked, 4'b0000, !tx_o, !rx_i, rst};

endmodule // muskoka_nexys3
