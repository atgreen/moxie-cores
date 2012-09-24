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

module muskoka (/*AUTOARG*/
  // Outputs
  uart_txd_o,
  // Inputs
  rst_i, clk_i
  );
   
  // --- Clock and Reset ------------------------------------------
  input  rst_i, clk_i;
  reg 	 rst;

  // --- UART -----------------------------------------------------
  output  uart_txd_o;

  // Always zero
  wire [0:0] zero = 0;
  
  // Moxie/Wishbone interface
  wire [31:0] iw2mx_dat;
  wire [31:0] mx2iw_dat;
  wire [31:0] mx2iw_adr;
  wire [1:0]  iw2mx_sel;
  wire 	      mx2iw_we;
  wire 	      mx2iw_cyc;
  wire        mx2iw_stb;
  wire 	      iw2mx_ack;

  wire [31:0] dw2mx_dat;
  wire [31:0] mx2dw_dat;
  wire [31:0] mx2dw_adr;
  wire [1:0]  dw2mx_sel;
  wire 	      mx2dw_we;
  wire 	      mx2dw_cyc;
  wire        mx2dw_stb;
  wire 	      dw2mx_ack;
   
  // Bootrom/Wishbone interface
  wire [31:0] iw2br_dat;
  wire [31:0] br2iw_dat;
  wire [31:0] iw2br_adr;
  wire [1:0]  iw2br_sel;
  wire 	      iw2br_we;
  wire 	      iw2br_cyc;
  wire        iw2br_stb;
  wire 	      br2iw_ack;

  // synthesis translate_off
  initial
    begin
      $dumpvars(1,core);
      $dumpvars(1,insn_intercon);
      $dumpvars(1,data_intercon);
      $dumpvars(1,rom);
    end
  // synthesis translate_on

  // slave 0 - bootrom @ 0x1000 for 512 bytes
  // slave 1 - unused
  // slave 2 - unused
  // slave 3 - unused
  
  wb_intercon #(.data_width (32),
		.slave_0_mask (32'b1111_1111_1111_1111_1111_0000_0000_0000),
	        .slave_0_addr (32'b0000_0000_0000_0000_0001_0000_0000_0000),
		.slave_1_mask (32'b0000_0000_0000_0000_0000_0000_0000_0000),
	        .slave_1_addr (32'b1111_1111_1111_1111_1111_1111_1111_1111),
		.slave_2_mask (32'b0000_0000_0000_0000_0000_0000_0000_0000),
	        .slave_2_addr (32'b1111_1111_1111_1111_1111_1111_1111_1111),
		.slave_3_mask (32'b0000_0000_0000_0000_0000_0000_0000_0000),
	        .slave_3_addr (32'b1111_1111_1111_1111_1111_1111_1111_1111))

  insn_intercon (.wbm_dat_o (iw2mx_dat),
		 .wbm_adr_i (mx2iw_adr),
		 .wbm_sel_i (iw2mx_sel),
		 .wbm_we_i (mx2iw_we),
		 .wbm_cyc_i (mx2iw_cyc),
		 .wbm_stb_i (mx2iw_stb),
		 .wbm_ack_o (iw2mx_ack),
		 
		 .wbs_0_dat_o (br2iw_dat),
		 .wbs_0_adr_o (iw2br_adr),
		 .wbs_0_sel_o (iw2br_sel),
		 .wbs_0_we_o (iw2br_we),
		 .wbs_0_cyc_o (iw2br_cyc),
		 .wbs_0_stb_o (iw2br_stb),
		 .wbs_0_ack_i (br2iw_ack),

		 .wbs_1_ack_i (zero),
		 .wbs_2_ack_i (zero),
		 .wbs_3_ack_i (zero));

    
  wb_intercon #(.slave_0_mask (32'b1111_1111_1111_1111_1111_0000_0000_0000),
	        .slave_0_addr (32'b0000_0100_0000_0000_0000_0000_0000_0000),
		.slave_1_mask (32'b0000_0000_0000_0000_0000_0000_0000_0000),
	        .slave_1_addr (32'b1111_1111_1111_1111_1111_1111_1111_1111),
		.slave_2_mask (32'b0000_0000_0000_0000_0000_0000_0000_0000),
	        .slave_2_addr (32'b1111_1111_1111_1111_1111_1111_1111_1111),
		.slave_3_mask (32'b0000_0000_0000_0000_0000_0000_0000_0000),
	        .slave_3_addr (32'b1111_1111_1111_1111_1111_1111_1111_1111))

  data_intercon (.wbm_dat_o (dw2mx_dat),
		 .wbm_dat_i (mx2dw_dat),
		 .wbm_adr_i (mx2dw_adr),
		 .wbm_sel_i (dw2mx_sel),
		 .wbm_we_i (mx2dw_we),
		 .wbm_cyc_i (mx2dw_cyc),
		 .wbm_stb_i (mx2dw_stb),
		 .wbm_ack_o (dw2mx_ack),
		 
		 .wbs_0_dat_o (dw2ua_dat),
		 .wbs_0_dat_i (ua2dw_dat),
		 .wbs_0_adr_o (dw2ua_adr),
		 .wbs_0_sel_o (dw2ua_sel),
		 .wbs_0_we_o (dw2ua_we),
		 .wbs_0_cyc_o (dw2ua_cyc),
		 .wbs_0_stb_o (dw2ua_stb),
		 .wbs_0_ack_i (ua2dw_ack),

		 .wbs_1_ack_i (zero),
		 .wbs_2_ack_i (zero),
		 .wbs_3_ack_i (zero));

  uart_top uart (.wb_dat_i (dw2ua_dat),
		 .wb_dat_o (ua2dw_dat),
		 .wb_adr_i (dw2ua_adr),
		 .wb_sel_i (dw2ua_sel),
		 .wb_we_i (dw2ua_we),
		 .wb_cyc_i (dw2ua_cyc),
		 .wb_stb_i (dw2ua_stb),
		 .wb_ack_o (ua2dw_ack));
  
  bootrom rom (.wb_dat_i (iw2br_dat),
	       .wb_dat_o (br2iw_dat),
	       .wb_adr_i (iw2br_adr),
	       .wb_sel_i (iw2br_sel),
	       .wb_we_i (iw2br_we),
	       .wb_cyc_i (iw2br_cyc),
	       .wb_stb_i (iw2br_stb),
	       .wb_ack_o (br2iw_ack));

  moxie core (.rst_i (rst_i),
	      .clk_i (clk_i),

	      .wb_I_dat_i (iw2mx_dat),
	      .wb_I_dat_o (mx2iw_dat),
	      .wb_I_adr_o (mx2iw_adr),
	      .wb_I_sel_i (iw2mx_sel),
	      .wb_I_we_o (mx2iw_we),
	      .wb_I_cyc_o (mx2iw_cyc),
	      .wb_I_stb_o (mx2iw_stb),
	      .wb_I_ack_i (iw2mx_ack) ,

	      .wb_D_dat_i (dw2mx_dat),
	      .wb_D_dat_o (mx2dw_dat),
	      .wb_D_adr_o (mx2dw_adr),
	      .wb_D_sel_i (dw2mx_sel),
	      .wb_D_we_o (mx2dw_we),
	      .wb_D_cyc_o (mx2dw_cyc),
	      .wb_D_stb_o (mx2dw_stb),
	      .wb_D_ack_i (dw2mx_ack));

endmodule // muskoka
