// icache.v - Direct Mapped Instruction Cache
//
// Copyright (c) 2014, 2015, 2016, 2017, 2025  Anthony Green
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

`timescale 1ns / 1ps

// Moxie instruction cache.
//
// Moxie instructions are 16 bits in length with an optional immediate
// value of either 16 or 32 bits.  This simple direct mapped cache
// provides all 48 bits even if they may not all be required.  This
// means that there may be times when we load a redundant cache line
// from RAM.
//
// Does not latch output from cache!
//
// rst_i clears the cache valid bits for safety.
//
// The cache is indexed and tagged with virtual addresses.  A line's
// tag also records whether translation was on and whether the core
// was in user mode when it was filled, so the untranslated code an
// exception handler runs can never be mistaken for translated code at
// the same address, and user code can never run a line that was only
// fetched with supervisor rights.  Changing or removing a translation
// does not touch the cache; software has to flush it.
//
// Before filling a line the cache presents its address on
// xlate_vaddr_o.  If that cannot be translated the fill does not
// start and fault_o says so, for as long as the fetch address stays
// where it is.

module icache (/*AUTOARG*/
               // Outputs
               hit_o, inst_o, data_o, wb_adr_o, wb_sel_o, wb_cyc_o, wb_stb_o,
               xlate_vaddr_o, fault_o,
               // Inputs
               rst_i, clk_i, adr_i, stb_i, wb_dat_i, wb_ack_i, mode_i,
               xlate_ppn_i, xlate_fault_i, flush_i
               );

  input         rst_i,     clk_i;

  // Address to read from
  input  [31:0] adr_i;
  input         stb_i;

  // Cache hit indicator, instruction, data.
  output        hit_o;
  output [15:0] inst_o;
  output [31:0] data_o;

  // Interface to external memory.
  output reg [31:0] wb_adr_o;
  input  [15:0]     wb_dat_i;
  output [1:0]      wb_sel_o;
  output            wb_cyc_o;
  output reg        wb_stb_o;
  input             wb_ack_i;

  // {translation on, user mode}
  input  [1:0]      mode_i;

  // Translation of the line we want to fill next.
  output [31:0]     xlate_vaddr_o;
  input  [19:0]     xlate_ppn_i;
  input             xlate_fault_i;
  output            fault_o;

  // Forget everything.
  input             flush_i;

  assign wb_cyc_o = wb_stb_o;
  assign wb_sel_o = 2'b11;

  // 8 KiB I-cache, 32-byte lines => 256 lines, 16 halfwords per line.
  // Tag bits: 32 - (8 index) - (4 byte offset) - (1 alignment bit) = 19.

  reg               valid[0:255];
  reg  [20:0]       tags [0:255];
  reg  [15:0]       line [0:4095];

  // Base tag / word offset within a line (halfword granularity)
  wire [18:0] tag0 = adr_i[31:13];
  wire [3:0]  woff = adr_i[4:1];

  // Whether the 48-bit fetch crosses into next cache line(s)
  wire carry1 = (woff == 4'hF);  // +1 crosses
  wire carry2 = (woff >= 4'hE);  // +2 crosses

  // 9-bit index arithmetic to detect wrap 255->0; propagate carry into tag
  wire [8:0] set0_w = {1'b0, adr_i[12:5]};
  wire [8:0] set1_w = set0_w + {8'd0, carry1};
  wire [8:0] set2_w = set0_w + {8'd0, carry2};

  wire [7:0] set0 = set0_w[7:0];
  wire [7:0] set1 = set1_w[7:0];
  wire [7:0] set2 = set2_w[7:0];

  wire [18:0] tag1 = tag0 + set1_w[8];
  wire [18:0] tag2 = tag0 + set2_w[8];

  // Hit checks per potentially referenced line (each with its own tag)
  wire hit0 = valid[set0] & (tags[set0] == {mode_i, tag0});
  wire hit1 = valid[set1] & (tags[set1] == {mode_i, tag1});
  wire hit2 = valid[set2] & (tags[set2] == {mode_i, tag2});

  assign hit_o = (!rst_i) & hit0 & hit1 & hit2;

  // Indices into the line memory
  wire [11:0] base_idx0 = { set0, 4'b0000 };
  wire [11:0] base_idx1 = { set1, 4'b0000 };
  wire [11:0] base_idx2 = { set2, 4'b0000 };

  wire [11:0] inst_idx  = base_idx0 + {8'b0, woff};
  wire [11:0] data1_idx = carry1 ? base_idx1 : (inst_idx + 12'd1);
  wire [11:0] data2_idx = carry2 ? (carry1 ? (base_idx2 + 12'd1) : base_idx2) :
                          (carry1 ? (base_idx1 + 12'd1)
                                  : (inst_idx + 12'd2));

  // Outputs from cache memory
  assign inst_o        = line[inst_idx];
  assign data_o[31:16] = line[data1_idx];
  assign data_o[15:0]  = line[data2_idx];

  // --- Line fill ---------------------------------------------------
  parameter ICACHE_IDLE      = 2'd0;
  parameter ICACHE_FILL      = 2'd1;
  parameter ICACHE_FILL_WAIT = 2'd2;

  reg [1:0] state = ICACHE_IDLE;
  reg [3:0] count = 4'd0;

  // The line being filled, and the mode it is being filled in.
  reg [7:0]  hold_set;
  reg [18:0] hold_tag;
  reg [1:0]  hold_mode;

  // Fill whichever of the lines we need comes first.  Each fill ends
  // back in idle, where the next one is chosen and translated afresh.
  wire        miss = stb_i & !(hit0 & hit1 & hit2);
  wire [7:0]  fill_set = !hit0 ? set0 : (!hit1 ? set1 : set2);
  wire [18:0] fill_tag = !hit0 ? tag0 : (!hit1 ? tag1 : tag2);
  wire [31:0] fill_vaddr = { fill_tag, fill_set, 5'b00000 };

  assign xlate_vaddr_o = fill_vaddr;
  assign fault_o = (state == ICACHE_IDLE) & miss & xlate_fault_i;

  integer i;
  always @(posedge clk_i) begin
    if (rst_i | flush_i) begin
      state    <= ICACHE_IDLE;
      count    <= 4'd0;
      wb_stb_o <= 1'b0;
      wb_adr_o <= 32'd0;
      for (i = 0; i < 256; i = i + 1)
        valid[i] <= 1'b0;

      hold_set <= 8'd0;
      hold_tag <= 19'd0;
      hold_mode <= 2'd0;

    end else begin
      case (state)

        ICACHE_IDLE: begin
          count <= 4'd0;

          if (miss & !xlate_fault_i) begin
            hold_set  <= fill_set;
            hold_tag  <= fill_tag;
            hold_mode <= mode_i;

            // The line stops being what its tag says as soon as we
            // start overwriting it.
            valid[fill_set] <= 1'b0;

            // A line never crosses a page, so one translation does
            // for the whole fill.
            wb_adr_o <= mode_i[1] ? { xlate_ppn_i, fill_vaddr[11:0] } : fill_vaddr;
            wb_stb_o <= 1'b1;
            state    <= ICACHE_FILL_WAIT;
          end else begin
            wb_stb_o <= 1'b0;
          end
        end

        ICACHE_FILL: begin
          if (wb_ack_i) begin
            line[ ({hold_set,4'b0000} + {8'b0, count}) ] <= wb_dat_i;
            wb_adr_o <= wb_adr_o + 32'd2;
            count    <= count + 4'd1;

            if (count == 4'd15) begin
              tags[hold_set]  <= { hold_mode, hold_tag };
              valid[hold_set] <= 1'b1;
              count           <= 4'd0;
              wb_stb_o        <= 1'b0;
              state           <= ICACHE_IDLE;
            end else begin
              state <= ICACHE_FILL_WAIT;
            end
          end
        end

        ICACHE_FILL_WAIT: begin
          state <= ICACHE_FILL;
        end

        default: begin
          state <= ICACHE_IDLE;
        end

      endcase
    end
  end

endmodule // icache
