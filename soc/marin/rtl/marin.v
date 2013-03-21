// marin.v - Top level Marin SoC module.
//
// Copyright (c) 2012, 2013  Anthony Green.  All Rights Reserved.
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


module marin (/*AUTOARG*/
   // Outputs
   seg, an, tx_o, leds_o,
   // Inputs
   rst_i, clk_100mhz_i, btnl, rx_i
   );
 
  // --- Clock and Reset ------------------------------------------
  input  rst_i, clk_100mhz_i;
  reg 	 rst;

   input btnl;
   
   
  // -- Seven Segment Display -------------------------------------
  output [7:0] seg;
  output [3:0] an;

  // -- UART ------------------------------------------------------
  output tx_o;
  input rx_i;

  // -- LEDs ------------------------------------------------------
  output [7:0] leds_o;

  // -- psram -----------------------------------------------------
  // output [22:0] mem_addr;
  // output        mem_clk;
  // output        mem_cen;
  // output        mem_cre;
  // output        mem_oen;
  // output        mem_wen;
  // output        mem_adv;
  // input [15:0]  mem_data_i;
  // output [15:0] mem_data_o;
  // inout [15:0]  mem_data;

  // MoxieLite IRQ Line
  wire [7:0] 	pi2mx_irq;
   
   
  // MoxieLite/Wishbone interface
  wire [15:0] wb2mx_dat;
  wire [15:0] mx2wb_dat;
  wire [31:0] mx2wb_adr;
  wire [1:0]  mx2wb_sel;
  wire        mx2wb_we;
  wire        mx2wb_cyc;
  wire        mx2wb_stb;
  wire        wb2mx_ack;
   
  // Bootrom/Wishbone interface
  wire [15:0] wb2br_dat;
  wire [15:0] br2wb_dat;
  wire [31:0] wb2br_adr;
  wire [1:0]  wb2br_sel;
  wire 	      wb2br_we;
  wire 	      wb2br_cyc;
  wire        wb2br_stb;
  wire 	      br2wb_ack;

  // Display/Wishbone interface
  wire [15:0] wb2dp_dat;
  wire [31:0] wb2dp_adr;
  wire [1:0]  wb2dp_sel;
  wire 	      wb2dp_we;
  wire 	      wb2dp_cyc;
  wire        wb2dp_stb;
  wire 	      dp2wb_ack;

  // UART/Wishbone interface
  wire [15:0] wb2ua_dat;
  wire [15:0] ua2wb_dat;
  wire [31:0] wb2ua_adr;
  wire [1:0]  wb2ua_sel;
  wire 	      wb2ua_we;
  wire 	      wb2ua_cyc;
  wire        wb2ua_stb;
  wire 	      ua2wb_ack;

  // RAM/Wishbone interface
  wire [15:0] wb2rm_dat;
  wire [15:0] rm2wb_dat;
  wire [31:0] wb2rm_adr;
  wire [1:0]  wb2rm_sel;
  wire 	      wb2rm_we;
  wire 	      wb2rm_cyc;
  wire        wb2rm_stb;
  wire 	      rm2wb_ack;

  // Cellular RAM Wishbone interface
  wire [15:0] wb2cr_dat;
  wire [15:0] cr2wb_dat;
  wire [31:0] wb2cr_adr;
  wire [1:0]  wb2cr_sel;
  wire 	      wb2cr_we;
  wire 	      wb2cr_cyc;
  wire        wb2cr_stb;
  wire 	      cr2wb_ack;

  // Programmable interrupt controller
  wire [15:0] wb2pi_dat;
  wire [15:0] pi2wb_dat;
  wire [31:0] wb2pi_adr;
  wire [1:0]  wb2pi_sel;
  wire 	      wb2pi_we;
  wire 	      wb2pi_cyc;
  wire        wb2pi_stb;
  wire 	      pi2wb_ack;

  // Programmable timer
  wire [15:0] wb2ti_dat;
  wire [15:0] ti2wb_dat;
  wire [31:0] wb2ti_adr;
  wire [1:0]  wb2ti_sel;
  wire 	      wb2ti_we;
  wire 	      wb2ti_cyc;
  wire        wb2ti_stb;
  wire 	      ti2wb_ack;

  wire      clk_cpu;
  wire      clk_100mhz;

  clk_wiz_v3_6 clockgen (.CLK_IN1 (clk_100mhz_i),
			 .RESET (rst_i),
			 .CLK_OUT1 (clk_cpu),
			 .CLK_OUT2 (clk_100mhz));
    
  wb_intercon #(.data_width (16),
		/* ROM */
		.slave_0_mask (32'b1111_1111_1111_1111_1111_0000_0000_0000),
	        .slave_0_addr (32'b0000_0000_0000_0000_0001_0000_0000_0000),
		/* 7-Segment Display */
		.slave_1_mask (32'b1111_1111_1111_1111_1111_1111_1111_1100),
	        .slave_1_addr (32'b1111_0000_0000_0000_0000_0000_0000_0000),
		/* UART */
		.slave_2_mask (32'b1111_1111_1111_1111_1111_1111_1111_1100),
	        .slave_2_addr (32'b1111_0000_0000_0000_0000_0000_0000_0100),
		/* Boot ROM  - 1k @ 0x100000000 */
		.slave_3_mask (32'b1111_1111_1111_1111_1111_0000_0000_0000),
	        .slave_3_addr (32'b0001_0000_0000_0000_0000_0000_0000_0000),
		/* Cellular RAM - 16MB @ 0x300000000 */
		.slave_4_mask (32'b1111_1111_0000_0000_0000_0000_0000_0000),
	        .slave_4_addr (32'b0011_0000_0000_0000_0000_0000_0000_0000),
		/* PIC @ 0xF0000008 */
		.slave_5_mask (32'b1111_1111_1111_1111_1111_1111_1111_1100),
	        .slave_5_addr (32'b1111_0000_0000_0000_0000_0000_0000_1000),
		/* PIT @ 0xF000000C */
		.slave_6_mask (32'b1111_1111_1111_1111_1111_1111_1111_1100),
	        .slave_6_addr (32'b1111_0000_0000_0000_0000_0000_0000_1100))

  bus_intercon (.wbm_dat_o (wb2mx_dat),
		.wbm_dat_i (mx2wb_dat),
		.wbm_adr_i (mx2wb_adr),
		.wbm_sel_i (mx2wb_sel),
		.wbm_we_i (mx2wb_we),
		.wbm_cyc_i (mx2wb_cyc),
		.wbm_stb_i (mx2wb_stb),
		.wbm_ack_o (wb2mx_ack),
		
		.wbs_0_dat_o (),
		.wbs_0_dat_i (br2wb_dat),
		.wbs_0_adr_o (wb2br_adr),
		.wbs_0_sel_o (wb2br_sel),
		.wbs_0_we_o (wb2br_we),
		.wbs_0_cyc_o (wb2br_cyc),
		.wbs_0_stb_o (wb2br_stb),
		.wbs_0_ack_i (br2wb_ack),
		
		.wbs_1_dat_o (wb2dp_dat),
		.wbs_1_dat_i (),
		.wbs_1_adr_o (wb2dp_adr),
		.wbs_1_sel_o (wb2dp_sel),
		.wbs_1_we_o (wb2dp_we),
		.wbs_1_cyc_o (wb2dp_cyc),
		.wbs_1_stb_o (wb2dp_stb),
		.wbs_1_ack_i (dp2wb_ack),
		
		.wbs_2_dat_o (wb2ua_dat),
		.wbs_2_dat_i (ua2wb_dat),
		.wbs_2_adr_o (wb2ua_adr),
		.wbs_2_sel_o (wb2ua_sel),
		.wbs_2_we_o (wb2ua_we),
		.wbs_2_cyc_o (wb2ua_cyc),
		.wbs_2_stb_o (wb2ua_stb),
		.wbs_2_ack_i (ua2wb_ack),

		.wbs_3_dat_o (wb2rm_dat),
		.wbs_3_dat_i (rm2wb_dat),
		.wbs_3_adr_o (wb2rm_adr),
		.wbs_3_sel_o (wb2rm_sel),
		.wbs_3_we_o (wb2rm_we),
		.wbs_3_cyc_o (wb2rm_cyc),
		.wbs_3_stb_o (wb2rm_stb),
		.wbs_3_ack_i (rm2wb_ack), 

		.wbs_4_dat_o (wb2cr_dat),
		.wbs_4_dat_i (cr2wb_dat),
		.wbs_4_adr_o (wb2cr_adr),
		.wbs_4_sel_o (wb2cr_sel),
		.wbs_4_we_o (wb2cr_we),
		.wbs_4_cyc_o (wb2cr_cyc),
		.wbs_4_stb_o (wb2cr_stb),
		.wbs_4_ack_i (cr2wb_ack), 

		.wbs_5_dat_o (wb2pi_dat),
		.wbs_5_dat_i (pi2wb_dat),
		.wbs_5_adr_o (wb2pi_adr),
		.wbs_5_sel_o (wb2pi_sel),
		.wbs_5_we_o (wb2pi_we),
		.wbs_5_cyc_o (wb2pi_cyc),
		.wbs_5_stb_o (wb2pi_stb),
		.wbs_5_ack_i (pi2wb_ack),
  
		.wbs_6_dat_o (wb2ti_dat),
		.wbs_6_dat_i (ti2wb_dat),
		.wbs_6_adr_o (wb2ti_adr),
		.wbs_6_sel_o (wb2ti_sel),
		.wbs_6_we_o (wb2ti_we),
		.wbs_6_cyc_o (wb2ti_cyc),
		.wbs_6_stb_o (wb2ti_stb),
		.wbs_6_ack_i (ti2wb_ack)); 
  
   wire       br_debug;
   wire [7:0] ml_debug;

  wire [31:0] watchdog_adr;
  wire 	      watchdog_fault;

  // Wishbone Bus Watchdog.
  wb_watchdog watchdog (.clk_i (clk_cpu),
			.rst_i (rst_i),
			.wbm_adr_i (mx2wb_adr),
			.wbm_stb_i (mx2wb_stb),
			.wbm_ack_i (wb2mx_ack),
			.adr_o (watchdog_adr),
			.fault_o (watchdog_fault));

  // Wishbone bus slaves.
  bootrom16 rom (.clk_i (clk_cpu),
		 .wb_dat_i (),
		 .wb_dat_o (br2wb_dat),
		 .wb_adr_i (wb2br_adr),
		 .wb_sel_i (wb2br_sel),
		 .wb_we_i (wb2br_we),
		 .wb_cyc_i (wb2br_cyc),
		 .wb_stb_i (wb2br_stb),
		 .wb_ack_o (br2wb_ack));

  ram16bit_wb ram (.clk_i (clk_cpu),
		   .wb_dat_i (wb2rm_dat),
		   .wb_dat_o (rm2wb_dat),
		   .wb_adr_i (wb2rm_adr),
		   .wb_sel_i (wb2rm_sel),
		   .wb_we_i (wb2rm_we),
		   .wb_cyc_i (wb2rm_cyc),
		   .wb_stb_i (wb2rm_stb),
		   .wb_ack_o (rm2wb_ack));

  simple_pic pic (.clk_i (clk_cpu),
		  .rst_i (rst_i),
		  .cyc_i (wb2pi_cyc),
		  .stb_i (wb2pi_stb),
		  .we_i (wb2pi_we),
		  .adr_i (wb2pi_adr[1:0]),
		  .dat_i (wb2pi_dat[7:0]),
		  .ack_o (pi2wb_ack),
		  .dat_o (pi2wb_dat[7:0]),
		  .irq (pi2mx_irq));
   
  // psram_wb cellram (.clk_i (clk_cpu),
  // 		    // Wishbone Interface
  // 		    .wb_dat_i (wb2cr_dat),
  // 		    .wb_dat_o (cr2wb_dat),
  // 		    .wb_adr_i (wb2cr_adr),
  // 		    .wb_sel_i (wb2cr_sel),
  // 		    .wb_we_i (wb2cr_we),
  // 		    .wb_cyc_i (wb2cr_cyc),
  // 		    .wb_stb_i (wb2cr_stb),
  // 		    .wb_ack_o (cr2wb_ack),
  // 		    // External Interface
  // 		    .mem_addr (mem_addr),
  // 		    .mem_clk (mem_clk),
  // 		    .mem_cen (mem_cen),
  // 		    .mem_cre (mem_cre),
  // 		    .mem_oen (mem_oen),
  // 		    .mem_wen (mem_wen),
  // 		    .mem_adv (mem_adv),
  // 		    .mem_wait (mem_wait),
  // 		    .mem_data_i (mem_data_i),
  // 		    .mem_data_o (mem_data_o),
  // 		    .mem_data_t (mem_data));

   wire       ti_pit;
   wire       ti_pit_irq;
   
  pit_top #(.DWIDTH (16)) pit
              (.wb_dat_o (ti2wb_dat),
  	       .wb_ack_o (ti2wb_ack),
  	       .wb_clk_i (clk_cpu),
  	       .wb_rst_i (rst_i),
  	       .arst_i (1'b0),
  	       .wb_adr_i (wb2ti_adr[2:0]),
  	       .wb_dat_i (wb2ti_dat),
  	       .wb_we_i (wb2ti_we),
  	       .wb_stb_i (wb2ti_stb),
  	       .wb_cyc_i (wb2ti_cyc),
  	       .wb_sel_i (wb2ti_sel),
  	       .pit_o (ti_pit),
  	       .pit_irq_o (ti_pit_irq));

  wire [12:0]  gdbdebug;

  nexys7seg_wb disp (.rst_i (rst_i),
		     .clk_i (clk_cpu),
		     .wb_dat_i (wb2dp_dat),
//		     .wb_dat_i ({gdbdebug[12:5], gdbdebug[12:5]}),
		     .wb_sel_i (wb2dp_sel),
		     .wb_we_i (wb2dp_we),
		     .wb_cyc_i (wb2dp_cyc),
		     .wb_stb_i (wb2dp_stb),
		     .wb_ack_o (dp2wb_ack),
  		     .clk_100mhz_i (clk_cpu),
		     .seg (seg),
		     .an (an)); 

  uart_wb uart (.rst_i (rst_i),
		.clk_i (clk_cpu),
		.wb_dat_i (wb2ua_dat),
		.wb_dat_o (ua2wb_dat),
		.wb_sel_i (wb2ua_sel),
		.wb_we_i (wb2ua_we),
		.wb_cyc_i (wb2ua_cyc),
		.wb_stb_i (wb2ua_stb),
		.wb_ack_o (ua2wb_ack),
		.rx_i (),
		.tx_o ());

  wire [1:0]	       gdb2mx;
  
  moxielite_wb core (.rst_i (rst_i),
		     .clk_i (clk_cpu),
		     .wb_dat_i (wb2mx_dat),
		     .wb_dat_o (mx2wb_dat),
		     .wb_adr_o (mx2wb_adr),
		     .wb_sel_o (mx2wb_sel),
		     .wb_we_o (mx2wb_we),
		     .wb_cyc_o (mx2wb_cyc),
		     .wb_stb_o (mx2wb_stb),
		     .wb_ack_i (wb2mx_ack),
		     .gdb_i (gdb2mx),
		     .debug_o (ml_debug),
		     .irq_i (btnl));
//		     .irq_i (pi2mx_irq));

  statled status_led (.clk (clk_cpu),
		      .rst (rst_i),
		      .status (CODE_SIX),
		      .led (sled));

  wire [7:0]  debug;

  gdbte_uart gdb (.debug_o (debug),
		  .rst_i (rst_i),
		  .clk_i (clk_cpu),
		  .wb_dat_i (mx2wb_dat),
		  .wb_dat_o (),
		  .wb_adr_o (),
		  .wb_sel_o (),
		  .wb_we_o (),
		  .wb_cyc_o (),
		  .wb_stb_o (),
		  .wb_ack_i (),
		  .rx_i (rx_i),
		  .tx_o (tx_o),
		  .gdb_ctrl_o (gdb2mx));
          
   assign leds_o = ml_debug;

endmodule
