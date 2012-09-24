// muskoka.v - Top level Muskoka SoC module.
//
// Copyright (c) 2009, 2010, 2011  Anthony Green.  All Rights Reserved.
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


module muskoka_de20 (/*AUTOARG*/
  // Inputs
  clk50_i, rst_i, uart_txd_i
  );

  input clk50_i;
  input rst_i;
  input uart_txd_i;
  
  wire clk;
  
  // Phased lock loop
  pll pll (.inclk0 (clk50_i),
	   .c0     (clk));

  muskoka soc (.rst_i (rst_i),
	       .clk_i (clk),
	       .uart_txd_i (uart_txd_i));
  
endmodule // muskoka_de20
