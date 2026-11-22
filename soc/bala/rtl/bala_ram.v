// bala_ram.v - RAM for the Bala SoC
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

// RAM on mox125's 16-bit bus, preloaded from INIT, a file of 16-bit
// words for $readmemh, one per line from address zero.  The bus
// convention is the core's: a short is the byte at the address on the
// high lane and the one after it on the low lane; a byte is read on
// the high lane and written (sel 01) from the low lane.  The ack
// comes a clock after the request, and never twice running.

module bala_ram #(parameter BYTES = 8192,
		  parameter INIT = "") (
  input             clk_i,
  input             rst_i,
  input [31:0]      wb_adr_i,
  input [15:0]      wb_dat_i,
  output [15:0]     wb_dat_o,
  input [1:0]       wb_sel_i,
  input             wb_we_i,
  input             wb_cyc_i,
  input             wb_stb_i,
  output reg        wb_ack_o
  );

  localparam WORDS = BYTES / 2;
  localparam AW = $clog2(WORDS);

  reg [15:0] mem [0:WORDS-1];
  reg [15:0] q;
  reg        odd_q;

  initial
    if (INIT != "")
      $readmemh(INIT, mem);

  wire          request = wb_stb_i & wb_cyc_i & !wb_ack_o;
  wire [AW-1:0] index = wb_adr_i[AW:1];
  wire          odd = wb_adr_i[0];
  wire          byte_write = wb_sel_i == 2'b01;
  wire          write = request & wb_we_i;
  wire          write_hi = write & (!byte_write | !odd);
  wire          write_lo = write & (!byte_write | odd);

  always @(posedge clk_i)
    begin
      wb_ack_o <= !rst_i & request;
      q <= mem[index];
      odd_q <= odd;
      if (write_hi)
	mem[index][15:8] <= byte_write ? wb_dat_i[7:0] : wb_dat_i[15:8];
      if (write_lo)
	mem[index][7:0] <= wb_dat_i[7:0];
    end

  assign wb_dat_o = odd_q ? {q[7:0], q[15:8]} : q;

endmodule // bala_ram
