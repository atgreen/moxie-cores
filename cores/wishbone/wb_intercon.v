// wb_intercon.v - Wishbone Shared Bus Interconnect 
//
// Copyright (c) 2010, 2011, 2012  Anthony Green.  All Rights Reserved.
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


module wb_intercon #(
  parameter data_width = 32,
  parameter slave_0_mask = 20'h00000,
  parameter slave_0_addr = 20'h00000,
  parameter slave_1_mask = 20'h00000,
  parameter slave_1_addr = 20'h00000,
  parameter slave_2_mask = 20'h00000,
  parameter slave_2_addr = 20'h00000,
  parameter slave_3_mask = 20'h00000,
  parameter slave_3_addr = 20'h00000
  )(/*AUTOARG*/
  // Outputs
  wbm_dat_o, wbm_ack_o, wbs_0_dat_o, wbs_0_adr_o, wbs_0_sel_o,
  wbs_0_we_o, wbs_0_cyc_o, wbs_0_stb_o, wbs_1_dat_o, wbs_1_adr_o,
  wbs_1_sel_o, wbs_1_we_o, wbs_1_cyc_o, wbs_1_stb_o, wbs_2_dat_o,
  wbs_2_adr_o, wbs_2_sel_o, wbs_2_we_o, wbs_2_cyc_o, wbs_2_stb_o,
  wbs_3_dat_o, wbs_3_adr_o, wbs_3_sel_o, wbs_3_we_o, wbs_3_cyc_o,
  wbs_3_stb_o,
  // Inputs
  wbm_dat_i, wbm_adr_i, wbm_sel_i, wbm_we_i, wbm_cyc_i, wbm_stb_i,
  wbs_0_dat_i, wbs_0_ack_i, wbs_1_dat_i, wbs_1_ack_i, wbs_2_dat_i,
  wbs_2_ack_i, wbs_3_dat_i, wbs_3_ack_i
  );

  // Wishbone Master Interface
  input [data_width-1:0] wbm_dat_i;
  output [data_width-1:0] wbm_dat_o;
  input [31:0]  wbm_adr_i;
  input [1:0]   wbm_sel_i;
  input         wbm_we_i;
  input         wbm_cyc_i;
  input         wbm_stb_i;
  output        wbm_ack_o;
  
  // Wishbone Slave 0 Interface
  input [data_width-1:0]  wbs_0_dat_i;
  output [data_width-1:0] wbs_0_dat_o;
  output [31:0] wbs_0_adr_o;
  output [1:0]  wbs_0_sel_o;
  output        wbs_0_we_o;
  output        wbs_0_cyc_o;
  output        wbs_0_stb_o;
  input         wbs_0_ack_i;

  // Wishbone Slave 1 Interface
  input [data_width-1:0]  wbs_1_dat_i;
  output [data_width-1:0] wbs_1_dat_o;
  output [31:0] wbs_1_adr_o;
  output [1:0]  wbs_1_sel_o;
  output        wbs_1_we_o;
  output        wbs_1_cyc_o;
  output        wbs_1_stb_o;
  input         wbs_1_ack_i;

  // Wishbone Slave 2 Interface
  input [data_width-1:0]  wbs_2_dat_i;
  output [data_width-1:0] wbs_2_dat_o;
  output [31:0] wbs_2_adr_o;
  output [1:0]  wbs_2_sel_o;
  output        wbs_2_we_o;
  output        wbs_2_cyc_o;
  output        wbs_2_stb_o;
  input         wbs_2_ack_i;

  // Wishbone Slave 3 Interface
  input [data_width-1:0]  wbs_3_dat_i;
  output [data_width-1:0] wbs_3_dat_o;
  output [31:0] wbs_3_adr_o;
  output [1:0]  wbs_3_sel_o;
  output        wbs_3_we_o;
  output        wbs_3_cyc_o;
  output        wbs_3_stb_o;
  input         wbs_3_ack_i;

  // Slave select wires.
  wire 		slave_0_sel;
  wire 		slave_1_sel;
  wire 		slave_2_sel;
  wire 		slave_3_sel;

  assign slave_0_sel = ((wbm_adr_i & slave_0_mask) == slave_0_addr);
  assign slave_1_sel = ((wbm_adr_i & slave_1_mask) == slave_1_addr);
  assign slave_2_sel = ((wbm_adr_i & slave_2_mask) == slave_2_addr);
  assign slave_3_sel = ((wbm_adr_i & slave_3_mask) == slave_3_addr);

  // An aggregation of all master bus input wires
  wire [32+data_width+2+1+1-1:0] master_bus_i;
  
  assign master_bus_i = {wbm_adr_i, wbm_dat_i, wbm_sel_i, 
			 wbm_we_i, wbm_cyc_i};

  // Hook all of the slave devices up to the bus.

  assign {wbs_0_adr_o, wbs_0_dat_o, wbs_0_sel_o,
          wbs_0_we_o, wbs_0_cyc_o} = master_bus_i;
  assign wbs_0_stb_o = wbm_cyc_i & wbm_stb_i & slave_0_sel;
  
  assign {wbs_1_adr_o, wbs_1_dat_o, wbs_1_sel_o,
          wbs_1_we_o, wbs_1_cyc_o} = master_bus_i;
  assign wbs_1_stb_o = wbm_cyc_i & wbm_stb_i & slave_1_sel;
  
  assign {wbs_2_adr_o, wbs_2_dat_o, wbs_2_sel_o,
          wbs_2_we_o, wbs_2_cyc_o} = master_bus_i;
  assign wbs_2_stb_o = wbm_cyc_i & wbm_stb_i & slave_2_sel;
  
  assign {wbs_3_adr_o, wbs_3_dat_o, wbs_3_sel_o,
          wbs_3_we_o, wbs_3_cyc_o} = master_bus_i;
  assign wbs_3_stb_o = wbm_cyc_i & wbm_stb_i & slave_3_sel;
  
  // Master bus acknowlegement.
  assign wbm_ack_o = wbs_0_ack_i | wbs_1_ack_i | wbs_2_ack_i | wbs_3_ack_i;
  
  // Master bus data output comes from the selected slave.
  wire [data_width-1:0] i_dat_s;   // internal shared bus, slave data to master
   assign i_dat_s = wbs_0_dat_i;
 //({data_width{slave_0_sel}} & wbs_0_dat_o);
  assign wbm_dat_o = i_dat_s;
  //
  //                   | (slave_1_sel & wbs_1_dat_o)
  //                   | (slave_2_sel & wbs_2_dat_o)
  //                   | (slave_3_sel & wbs_3_dat_o);

endmodule // wb_intercon

