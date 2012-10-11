// marin.v - Top level Marin SoC module.
//
// Copyright (c) 2012  Anthony Green.  All Rights Reserved.
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


wire clock;

module marin (/*AUTOARG*/
   // Outputs
   seg, an,
   // Inputs
   rst_i, clk_i
   );


  // --- Clock and Reset ------------------------------------------
  input  rst_i, clk_i;
  reg 	 rst;

  output [7:0] seg;
  output [3:0] an;

  moxielite core (.clock (clk_i),
		  .reset_n (!rst),
		  .wait_n (1'b1));

  nexys7seg disp (.clk (clk_i),
		  .word (16'b1010101010101010),
		  .seg (seg),
		  .an (an));
   
endmodule
