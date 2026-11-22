// moxie_cpu.v - moxibit as the moxie:cpu interface
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

// moxie_cpu is what an SoC instantiates when it depends on the
// virtual core moxielogic:moxie-cores:cpu; the core it is built with,
// mox125 or moxibit, provides it.  Both have the same ports and the
// same 16-bit Wishbone bus.  The parameters are the union of theirs:
// mox125 uses BOOT_ADDRESS and ICACHE_SET_BITS, moxibit BOOT_ADDRESS,
// MDU and FAST_INC, and each ignores the others.

module moxie_cpu #(parameter BOOT_ADDRESS = 32'h00001000,
		   parameter ICACHE_SET_BITS = 8,
		   parameter MDU = 1,
		   parameter FAST_INC = 0) (
  input             clk_i,
  input             rst_i,

  output            wb_cyc_o,
  output            wb_stb_o,
  output            wb_we_o,
  output [31:0]     wb_adr_o,
  output [15:0]     wb_dat_o,
  output [1:0]      wb_sel_o,
  input [15:0]      wb_dat_i,
  input             wb_ack_i,

  input             irq_i
  );

  moxibit #(.BOOT_ADDRESS (BOOT_ADDRESS),
	    .MDU (MDU), .FAST_INC (FAST_INC))
  core (.clk_i (clk_i), .rst_i (rst_i),
	.wb_cyc_o (wb_cyc_o), .wb_stb_o (wb_stb_o), .wb_we_o (wb_we_o),
	.wb_adr_o (wb_adr_o), .wb_dat_o (wb_dat_o), .wb_sel_o (wb_sel_o),
	.wb_dat_i (wb_dat_i), .wb_ack_i (wb_ack_i),
	.irq_i (irq_i));

endmodule // moxie_cpu
