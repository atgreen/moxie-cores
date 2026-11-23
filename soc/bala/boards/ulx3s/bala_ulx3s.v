// bala_ulx3s.v - Bala on the Radiona ULX3S (ECP5)
//
// Copyright (c) 2026 Anthony Green.  All Rights Reserved.
// DO NOT ALTER OR REMOVE COPYRIGHT NOTICES.
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

// The 25 MHz oscillator clocks everything, and the serial port is the
// FTDI's at 115200 8N1.  The eight LEDs show display bits 7:0.

module bala_ulx3s #(parameter MDU = 1,
		    parameter COMPACT = 1,
		    parameter FAST_INC = 0) (
  input             clk_25mhz,
  input             ftdi_txd,
  output            ftdi_rxd,
  output [7:0]      led
  );

  // Held in reset for the first 32 clocks after configuration.
  reg [5:0] por = 6'd0;
  always @(posedge clk_25mhz)
    if (!por[5])
      por <= por + 6'd1;

  wire [15:0] display;

  bala #(.UART_DIVISOR (217), .MDU (MDU), .COMPACT (COMPACT), .FAST_INC (FAST_INC))
  soc (.clk_i (clk_25mhz), .rst_i (!por[5]),
       .rx_i (ftdi_txd), .tx_o (ftdi_rxd), .display_o (display));

  assign led = display[7:0];

endmodule // bala_ulx3s
