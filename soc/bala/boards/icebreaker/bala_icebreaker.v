// bala_icebreaker.v - Bala on the 1BitSquared iCEBreaker (iCE40 UP5K)
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

// The 12 MHz oscillator clocks everything, and the serial port is the
// FTDI's second channel at 115200 8N1.  The user button resets; the
// LEDs show display bits 0 (green) and 1 (red).  The core is
// whichever bala.core's flags choose; moxibit by default.

module bala_icebreaker #(parameter MDU = 1,
			 parameter FAST_INC = 0) (
  input             CLK,
  input             BTN_N,
  input             RX,
  output            TX,
  output            LEDG_N,
  output            LEDR_N
  );

  // Held in reset for the first 32 clocks after configuration.
  reg [5:0] por = 6'd0;
  always @(posedge CLK)
    if (!por[5])
      por <= por + 6'd1;

  wire [15:0] display;

  bala #(.UART_DIVISOR (104), .MDU (MDU), .FAST_INC (FAST_INC))
  soc (.clk_i (CLK), .rst_i (!por[5] | !BTN_N),
       .rx_i (RX), .tx_o (TX), .display_o (display));

  assign LEDG_N = !display[0];
  assign LEDR_N = !display[1];

endmodule // bala_icebreaker
