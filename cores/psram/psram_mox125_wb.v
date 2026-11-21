// psram_mox125_wb.v - 16-bit pseudo-static RAM for the mox125 bus.
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

// A Micron MT45W8MW16 or the like, used as plain asynchronous RAM:
// the clock, address-valid and configuration pins are tied off by
// whoever instantiates this.
//
// The memory is 16 bits wide and addressed in words.  The mox125 core
// addresses bytes, big-endian, and has its own idea of where a single
// byte travels on its 16-bit bus:
//
//   a word (sel 11) is the byte at the address, on the high lane, and
//     the byte after it;
//   a byte is read (sel 10) on the high lane, whichever address it
//     came from;
//   a byte is written (sel 01) from the low lane, wherever it is
//     going.
//
// So the byte at an even address is the memory's upper byte, the byte
// at an odd address is its lower byte, and single bytes are moved to
// and from the lane the core uses.
//
// An access holds the memory's strobes for STROBE_CLOCKS clocks, which
// has to cover its access time (70 ns) and the delays through the
// pins: two clocks is ample up to about 25 MHz.
//
// The core can abandon a read: an instruction fetch gives way to a
// data access at any time.  It tells an acknowledge for the old
// request from one for the new by their arriving a clock apart, which
// cannot work for a slave that takes several clocks, so this watches
// for the request changing under it and starts again when it does.

module psram_mox125_wb #(parameter STROBE_CLOCKS = 2,
			 // Address bits of the memory, in words: 23
			 // is 16 MiB.
			 parameter ADDR_BITS = 23
			 )(/*AUTOARG*/
   // Outputs
   wb_dat_o, wb_ack_o, mem_adr_o, mem_cen_o, mem_oen_o, mem_wen_o,
   mem_ubn_o, mem_lbn_o, mem_dat_o, mem_dat_oe_o,
   // Inputs
   rst_i, clk_i, wb_adr_i, wb_dat_i, wb_sel_i, wb_we_i, wb_cyc_i,
   wb_stb_i, mem_dat_i
   );

  input             rst_i, clk_i;

  // --- Wishbone -------------------------------------------------
  input [31:0]      wb_adr_i;
  output reg [15:0] wb_dat_o;
  input [15:0]      wb_dat_i;
  input [1:0]       wb_sel_i;
  input             wb_we_i;
  input             wb_cyc_i;
  input             wb_stb_i;
  output reg        wb_ack_o;

  // --- The memory.  All of its strobes are active low. ----------
  output reg [ADDR_BITS-1:0] mem_adr_o;
  output reg        mem_cen_o;
  output reg        mem_oen_o;
  output reg        mem_wen_o;
  output reg        mem_ubn_o;
  output reg        mem_lbn_o;
  input [15:0]      mem_dat_i;
  output reg [15:0] mem_dat_o;
  output reg        mem_dat_oe_o;

  localparam IDLE = 2'd0, STROBE = 2'd1, HOLD = 2'd2, ACK = 2'd3;

  reg [1:0]         state;
  reg [3:0]         count;

  // The request being served.
  reg [31:0]        adr;
  reg               we;
  reg [1:0]         sel;

  // What the memory is saying, a clock ago.
  reg [15:0]        mem_dat_q;

  always @(posedge clk_i)
    mem_dat_q <= mem_dat_i;

  wire              request = wb_stb_i & wb_cyc_i;
  wire              byte_write = wb_we_i & (wb_sel_i == 2'b01);
  wire              same = (request & (wb_adr_i == adr) & (wb_we_i == we)
			    & (wb_sel_i == sel));

  always @(posedge clk_i)
    if (rst_i) begin
      state <= IDLE;
      count <= 4'd0;
      wb_ack_o <= 1'b0;
      wb_dat_o <= 16'h0000;
      mem_cen_o <= 1'b1;
      mem_oen_o <= 1'b1;
      mem_wen_o <= 1'b1;
      mem_ubn_o <= 1'b1;
      mem_lbn_o <= 1'b1;
      mem_dat_oe_o <= 1'b0;
    end else
      case (state)
	IDLE: begin
	  wb_ack_o <= 1'b0;
	  mem_dat_oe_o <= 1'b0;
	  count <= 4'd0;
	  if (request) begin
	    adr <= wb_adr_i;
	    we <= wb_we_i;
	    sel <= wb_sel_i;
	    mem_adr_o <= wb_adr_i[ADDR_BITS:1];
	    mem_cen_o <= 1'b0;
	    mem_oen_o <= wb_we_i;
	    mem_wen_o <= !wb_we_i;
	    // A read always fetches the whole word.
	    mem_ubn_o <= byte_write & wb_adr_i[0];
	    mem_lbn_o <= byte_write & !wb_adr_i[0];
	    mem_dat_o <= byte_write ? {wb_dat_i[7:0], wb_dat_i[7:0]} : wb_dat_i;
	    mem_dat_oe_o <= wb_we_i;
	    state <= STROBE;
	  end
	end

	STROBE: begin
	  count <= count + 4'd1;
	  if (!we & !same) begin
	    // The core has gone on to something else.
	    mem_cen_o <= 1'b1;
	    mem_oen_o <= 1'b1;
	    state <= IDLE;
	  end else if (count == STROBE_CLOCKS - 1) begin
	    // Let go of the strobes.  A write happens here, and the
	    // address and data stay put for the clock after it.  What a
	    // read fetched is on its way through mem_dat_q.
	    mem_cen_o <= 1'b1;
	    mem_oen_o <= 1'b1;
	    mem_wen_o <= 1'b1;
	    mem_ubn_o <= 1'b1;
	    mem_lbn_o <= 1'b1;
	    state <= HOLD;
	  end
	end

	HOLD: begin
	  mem_dat_oe_o <= 1'b0;
	  // A byte goes back on the high lane.
	  wb_dat_o <= ((sel == 2'b10) & adr[0])
		      ? {mem_dat_q[7:0], mem_dat_q[7:0]} : mem_dat_q;
	  if (!we & !same)
	    state <= IDLE;
	  else begin
	    wb_ack_o <= 1'b1;
	    state <= ACK;
	  end
	end

	ACK: begin
	  wb_ack_o <= 1'b0;
	  state <= IDLE;
	end
      endcase

endmodule // psram_mox125_wb
