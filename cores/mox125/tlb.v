// tlb.v - Software-managed translation lookaside buffer
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

// A direct-mapped TLB with 64 entries and 4 KiB pages.  The entry for
// a page is chosen by the low six bits of its virtual page number, so
// software never picks an index: it writes an entry and the page
// number says where it goes.
//
// An entry is written as two words, in the format of the TLBHI and
// TLBLO special registers:
//
//   hi  [31:12] virtual page number   [7:0] address space id
//   lo  [31:12] physical page number  [4] global  [3] user
//                                     [1] writable  [0] valid
//
// A global entry matches whatever the current address space id is.
//
// The entries are not cleared at reset, so that they can live in
// distributed RAM.  Software must write all 64 of them before turning
// translation on.

module tlb (/*AUTOARG*/
   // Outputs
   hit_o, ppn_o, user_o, writable_o,
   // Inputs
   clk_i, we_i, hi_i, lo_i, vaddr_i, asid_i
   );

  input         clk_i;

  // --- Entry write ----------------------------------------------
  input         we_i;
  input [31:0]  hi_i;
  input [31:0]  lo_i;

  // --- Lookup, combinational ------------------------------------
  input [31:0]  vaddr_i;
  input [7:0]   asid_i;
  output        hit_o;
  output [19:0] ppn_o;
  output        user_o;
  output        writable_o;

  // {valid, global, user, writable, asid, tag, ppn}
  reg [45:0]    entries[0:63];

  wire [45:0]   entry = entries[vaddr_i[17:12]];
  wire          valid = entry[45];
  wire          global = entry[44];
  wire [7:0]    asid = entry[41:34];
  wire [13:0]   tag = entry[33:20];

  assign user_o = entry[43];
  assign writable_o = entry[42];
  assign ppn_o = entry[19:0];
  assign hit_o = valid & (tag == vaddr_i[31:18]) & (global | (asid == asid_i));

  always @(posedge clk_i)
    if (we_i)
      entries[hi_i[17:12]] <= {lo_i[0], lo_i[4], lo_i[3], lo_i[1],
                               hi_i[7:0], hi_i[31:18], lo_i[31:12]};

endmodule // tlb
