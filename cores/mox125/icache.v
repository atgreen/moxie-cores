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

module icache (/*AUTOARG*/
               // Outputs
               hit_o, inst_o, data_o, wb_adr_o, wb_sel_o, wb_cyc_o, wb_stb_o,
               // Inputs
               rst_i, clk_i, adr_i, stb_i, wb_dat_i, wb_ack_i
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

  assign wb_cyc_o = wb_stb_o;
  assign wb_sel_o = 2'b11;

  // 8 KiB I-cache, 32-byte lines => 256 lines, 16 halfwords per line.
  // Tag bits: 32 - (8 index) - (4 byte offset) - (1 alignment bit) = 19.

  reg               valid[0:255];
  reg  [18:0]       tags [0:255];
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
  wire hit0 = valid[set0] & (tags[set0] == tag0);
  wire hit1 = valid[set1] & (tags[set1] == tag1);
  wire hit2 = valid[set2] & (tags[set2] == tag2);

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

  // --- State machine states -----------------------------------------
  parameter ICACHE_IDLE       = 4'd0;
  parameter ICACHE_FILL0      = 4'd1;
  parameter ICACHE_FILL1      = 4'd2;
  parameter ICACHE_FILL2      = 4'd5;
  parameter ICACHE_FILL0_WAIT = 4'd3;
  parameter ICACHE_FILL1_WAIT = 4'd4;
  parameter ICACHE_FILL2_WAIT = 4'd6;

  reg [3:0] state = ICACHE_IDLE;
  reg [3:0] count = 4'd0;

  // Latched request context for fill flows
  reg [7:0]  hold_set0, hold_set1, hold_set2;
  reg [18:0] hold_tag0, hold_tag1, hold_tag2;

  // Which lines actually need filling for this request
  reg        need0, need1, need2;

  // Combinational "next-need" wires used at end of each fill
  wire n1_after_fill0 = need1 & (hold_set1 != hold_set0);
  wire n2_after_fill0 = need2 & (hold_set2 != hold_set0);

  wire n0_after_fill1 = need0 & (hold_set0 != hold_set1);
  wire n2_after_fill1 = need2 & (hold_set2 != hold_set1);

  wire n0_after_fill2 = need0 & (hold_set0 != hold_set2);
  wire n1_after_fill2 = need1 & (hold_set1 != hold_set2);

  integer i;
  always @(posedge clk_i) begin
    if (rst_i) begin
      state    <= ICACHE_IDLE;
      count    <= 4'd0;
      wb_stb_o <= 1'b0;
      wb_adr_o <= 32'd0;
      for (i = 0; i < 256; i = i + 1)
        valid[i] <= 1'b0;

      hold_set0 <= 8'd0; hold_set1 <= 8'd0; hold_set2 <= 8'd0;
      hold_tag0 <= 19'd0; hold_tag1 <= 19'd0; hold_tag2 <= 19'd0;
      need0 <= 1'b0; need1 <= 1'b0; need2 <= 1'b0;

    end else begin
      case (state)

        ICACHE_IDLE: begin
          count <= 4'd0;

          if (stb_i && !(hit0 & hit1 & hit2)) begin
            // Latch the miss context
            hold_set0 <= set0; hold_tag0 <= tag0;
            hold_set1 <= set1; hold_tag1 <= tag1;
            hold_set2 <= set2; hold_tag2 <= tag2;

            need0 <= !hit0;
            need1 <= !hit1;
            need2 <= !hit2;

            wb_stb_o <= 1'b1;

            // IMPORTANT: Use current combinational (tagX,setX) for the first fill
            if (!hit0) begin
              wb_adr_o <= { tag0, set0, 5'b00000 };
              state    <= ICACHE_FILL0_WAIT;
            end else if (!hit1) begin
              wb_adr_o <= { tag1, set1, 5'b00000 };
              state    <= ICACHE_FILL1_WAIT;
            end else begin
              wb_adr_o <= { tag2, set2, 5'b00000 };
              state    <= ICACHE_FILL2_WAIT;
            end
          end else begin
            wb_stb_o <= 1'b0;
            state    <= ICACHE_IDLE;
          end
        end

        // --- Fill line 0 ---------------------------------------------
        ICACHE_FILL0: begin
          if (wb_ack_i) begin
            line[ ({hold_set0,4'b0000} + {8'b0, count}) ] <= wb_dat_i;
            wb_adr_o <= wb_adr_o + 32'd2;
            count    <= count + 4'd1;

            if (count == 4'd15) begin
              tags[hold_set0]  <= hold_tag0;
              valid[hold_set0] <= 1'b1;
              count            <= 4'd0;

              // commit next needs
              need0 <= 1'b0;
              need1 <= n1_after_fill0;
              need2 <= n2_after_fill0;

              // choose next line to fill
              if (n1_after_fill0) begin
                wb_adr_o <= { hold_tag1, hold_set1, 5'b00000 };
                state    <= ICACHE_FILL1_WAIT;
              end else if (n2_after_fill0) begin
                wb_adr_o <= { hold_tag2, hold_set2, 5'b00000 };
                state    <= ICACHE_FILL2_WAIT;
              end else begin
                wb_stb_o <= 1'b0;
                state    <= ICACHE_IDLE;
              end
            end else begin
              state <= ICACHE_FILL0_WAIT;
            end
          end
        end

        ICACHE_FILL0_WAIT: begin
          state <= ICACHE_FILL0;
        end

        // --- Fill line 1 ---------------------------------------------
        ICACHE_FILL1: begin
          if (wb_ack_i) begin
            line[ ({hold_set1,4'b0000} + {8'b0, count}) ] <= wb_dat_i;
            wb_adr_o <= wb_adr_o + 32'd2;
            count    <= count + 4'd1;

            if (count == 4'd15) begin
              tags[hold_set1]  <= hold_tag1;
              valid[hold_set1] <= 1'b1;
              count            <= 4'd0;

              need1 <= 1'b0;
              need0 <= n0_after_fill1;
              need2 <= n2_after_fill1;

              if (n0_after_fill1) begin
                wb_adr_o <= { hold_tag0, hold_set0, 5'b00000 };
                state    <= ICACHE_FILL0_WAIT;
              end else if (n2_after_fill1) begin
                wb_adr_o <= { hold_tag2, hold_set2, 5'b00000 };
                state    <= ICACHE_FILL2_WAIT;
              end else begin
                wb_stb_o <= 1'b0;
                state    <= ICACHE_IDLE;
              end
            end else begin
              state <= ICACHE_FILL1_WAIT;
            end
          end
        end

        ICACHE_FILL1_WAIT: begin
          state <= ICACHE_FILL1;
        end

        // --- Fill line 2 ---------------------------------------------
        ICACHE_FILL2: begin
          if (wb_ack_i) begin
            line[ ({hold_set2,4'b0000} + {8'b0, count}) ] <= wb_dat_i;
            wb_adr_o <= wb_adr_o + 32'd2;
            count    <= count + 4'd1;

            if (count == 4'd15) begin
              tags[hold_set2]  <= hold_tag2;
              valid[hold_set2] <= 1'b1;
              count            <= 4'd0;

              need2 <= 1'b0;
              need0 <= n0_after_fill2;
              need1 <= n1_after_fill2;

              if (n0_after_fill2) begin
                wb_adr_o <= { hold_tag0, hold_set0, 5'b00000 };
                state    <= ICACHE_FILL0_WAIT;
              end else if (n1_after_fill2) begin
                wb_adr_o <= { hold_tag1, hold_set1, 5'b00000 };
                state    <= ICACHE_FILL1_WAIT;
              end else begin
                wb_stb_o <= 1'b0;
                state    <= ICACHE_IDLE;
              end
            end else begin
              state <= ICACHE_FILL2_WAIT;
            end
          end
        end

        ICACHE_FILL2_WAIT: begin
          state <= ICACHE_FILL2;
        end

        default: begin
          state <= ICACHE_IDLE;
        end

      endcase
    end
  end

endmodule // icache
