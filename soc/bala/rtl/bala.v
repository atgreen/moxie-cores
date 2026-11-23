// bala.v - The Bala SoC
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

// Bala is the smallest system that runs moxie software: a moxie core,
// RAM from address zero preloaded with a program, and at 0xF0000000
// the mox125_board peripherals (console and 16550 UART on one serial
// port, timer, interrupt controller and 16 bits of display).  That is
// the board bench/mox125-board, tools/mox125-iss and the Nexys 3 SoC
// have, so what runs there runs here, if it fits in the RAM.
//
// The core is moxie_cpu: whichever of mox125 and moxibit the build
// chooses (see bala.core).  The core starts at BOOT_ADDRESS, 0x1000.

module bala #(parameter RAM_BYTES = 8192,
	      parameter INIT = "hello.vh",
	      // Clocks to a serial bit.
	      parameter UART_DIVISOR = 434,
	      // Passed to the core: see moxie_cpu.v.
	      parameter MDU = 1,
	      parameter COMPACT = 1,
	      parameter FAST_INC = 0,
	      parameter ICACHE_SET_BITS = 3) (
  input             clk_i,
  input             rst_i,
  input             rx_i,
  output            tx_o,
  output [15:0]     display_o
  );

  wire        cyc, stb, we, ack, irq;
  wire [31:0] adr;
  wire [15:0] dat_w, dat_r;
  wire [1:0]  sel;

  moxie_cpu #(.MDU (MDU), .COMPACT (COMPACT), .FAST_INC (FAST_INC),
	      .ICACHE_SET_BITS (ICACHE_SET_BITS))
  cpu (.clk_i (clk_i), .rst_i (rst_i),
       .wb_cyc_o (cyc), .wb_stb_o (stb), .wb_we_o (we),
       .wb_adr_o (adr), .wb_dat_o (dat_w), .wb_sel_o (sel),
       .wb_dat_i (dat_r), .wb_ack_i (ack),
       .irq_i (irq));

  // 0xF0000000 and up is the board; the rest is RAM, repeating.
  wire        board_sel = adr[31:28] == 4'hf;
  wire [15:0] ram_dat, board_dat;
  wire        ram_ack, board_ack;

  bala_ram #(.BYTES (RAM_BYTES), .INIT (INIT))
  ram (.clk_i (clk_i), .rst_i (rst_i),
       .wb_adr_i (adr), .wb_dat_i (dat_w), .wb_dat_o (ram_dat),
       .wb_sel_i (sel), .wb_we_i (we),
       .wb_cyc_i (cyc), .wb_stb_i (stb & !board_sel),
       .wb_ack_o (ram_ack));

  mox125_board_wb #(.UART_DIVISOR (UART_DIVISOR))
  board (.clk_i (clk_i), .rst_i (rst_i),
	 .wb_adr_i (adr), .wb_dat_i (dat_w), .wb_dat_o (board_dat),
	 .wb_sel_i (sel), .wb_we_i (we),
	 .wb_cyc_i (cyc), .wb_stb_i (stb & board_sel),
	 .wb_ack_o (board_ack),
	 .irq_o (irq), .rx_i (rx_i), .tx_o (tx_o), .display_o (display_o));

  assign dat_r = board_sel ? board_dat : ram_dat;
  assign ack = ram_ack | board_ack;

endmodule // bala
