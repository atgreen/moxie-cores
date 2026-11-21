// muskoka_de2.v - The Muskoka SoC on the Altera DE2 board.
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

// The PLL divides the board's 50 MHz oscillator by four, so the SoC
// runs at 12.5 MHz.  That is deliberately slow: the core has not been
// through timing closure on this part, and its longest paths are long.
// The UART runs at 115200 baud.
//
// KEY0 is reset.  The four right-hand hex displays are the SoC's.

module muskoka_de2 (/*AUTOARG*/
   // Outputs
   hex0_o, hex1_o, hex2_o, hex3_o, tx_o,
   // Inputs
   clk_i, key0_i, rx_i
   );

  input        clk_i;     // 50 MHz
  input        key0_i;    // low while pressed

  output [6:0] hex0_o;
  output [6:0] hex1_o;
  output [6:0] hex2_o;
  output [6:0] hex3_o;

  output       tx_o;
  input        rx_i;

  wire         clk_cpu;
  wire         locked;

  pll pll (.inclk0 (clk_i),
	   .c0 (),
	   .c1 (),
	   .c2 (clk_cpu),
	   .locked (locked));

  // Hold reset until the PLL has locked, and let go of it in step
  // with the clock.
  reg [3:0]    rst_sync = 4'b1111;

  always @(posedge clk_cpu)
    rst_sync <= {rst_sync[2:0], !key0_i | !locked};

  // The DE2's Cyclone II has no RAM that can be read without a clock,
  // so the instruction cache is built from flip-flops and has to be
  // small: 8 lines is 256 bytes.
  muskoka #(.ICACHE_SET_BITS (3),
	    // 12.5 MHz / 115200, to within half a percent
	    .UART_DIVISOR (108)) soc (.rst_i (rst_sync[3]),
				      .clk_i (clk_cpu),
				      .hex0_o (hex0_o),
				      .hex1_o (hex1_o),
				      .hex2_o (hex2_o),
				      .hex3_o (hex3_o),
				      .tx_o (tx_o),
				      .rx_i (rx_i));

endmodule // muskoka_de2
