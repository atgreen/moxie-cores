// moxbit_rf.v - The register file of the bit-serial moxie core
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

// Two read ports and a write port, one bit each per clock, made from a
// single RAM with one read and one write port, as SERV does it: the
// RAM is two bits wide, and reads for port A and port B take turns.
// That fits in one block RAM.
//
// There are 32 registers of 32 bits.  0-15 are the moxie registers;
// the core uses the rest for the special registers and as scratch.
//
// A pass reads and writes bit k of its registers in clock k.  It is
// preceded by two clocks, PREP and then the one after it, that read
// the first word for each port.  From then on port A's next word is
// read in the even clocks and port B's in the odd ones, and each is
// held until both its bits have been used.  Port B can read half a
// turn round, from bit 16.  The bit written in an even clock waits for
// the one after it, and the two are written together.

module moxbit_rf (
  input            clk_i,
  input            prep_i,	// the two clocks before a pass
  input            first_i,	// the first of them
  input [4:0]      cnt_i,	// the bit of the pass
  input [4:0]      reg_a_i,
  input [4:0]      reg_b_i,
  input            half_b_i,
  output           ra_o,
  output           rb_o,
  input            we_i,
  input [4:0]      reg_w_i,
  input            wd_i
  );

  reg [1:0] mem [0:511];
  reg [1:0] rdata;
  reg [1:0] abuf;
  reg       bhi, wlo;

  wire       reading_b = prep_i ? !first_i : cnt_i[0];
  wire [3:0] word = prep_i ? 4'd0 : cnt_i[4:1] + 4'd1;
  wire [8:0] raddr = reading_b ? {reg_b_i, word ^ {half_b_i, 3'b0}}
		   : {reg_a_i, word};

  always @(posedge clk_i)
    begin
      rdata <= mem[raddr];
      if (reading_b)
	abuf <= rdata;
      else if (!first_i)
	bhi <= rdata[1];
      if (!cnt_i[0])
	wlo <= wd_i;
      if (we_i & cnt_i[0])
	mem[{reg_w_i, cnt_i[4:1]}] <= {wd_i, wlo};
    end

  assign ra_o = cnt_i[0] ? abuf[1] : abuf[0];
  assign rb_o = cnt_i[0] ? bhi : rdata[0];

endmodule // moxbit_rf
