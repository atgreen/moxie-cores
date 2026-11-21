// nexys3_soc.v - mox125 and the mox125_sim board, around the Nexys 3's RAM.
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

// Everything but the pins and the clock, so that it can be simulated.
// The memory map is the simulators', so that what runs on them runs
// here:
//
//   0x00000000  16 MiB of RAM, the board's cellular RAM
//   0xF0000000  console, interrupt controller, timer, display and
//               16550 UART: see cores/mox125_board
//   0xFFFF0000  4k of ROM holding the boot loader, where the core
//               starts
//   0xFFFF1000  4k of RAM for the boot loader's stack

module nexys3_soc #(// UART clocks to the bit
		    parameter UART_DIVISOR = 108,
		    // log2 of the number of 32-byte instruction cache lines
		    parameter ICACHE_SET_BITS = 5,
		    // Clocks the RAM's strobes are held for
		    parameter RAM_STROBE_CLOCKS = 2
		    )(/*AUTOARG*/
   // Outputs
   tx_o, display_o, mem_adr_o, mem_cen_o, mem_oen_o, mem_wen_o,
   mem_ubn_o, mem_lbn_o, mem_dat_o, mem_dat_oe_o,
   // Inputs
   rst_i, clk_i, rx_i, mem_dat_i
   );

  input         rst_i, clk_i;

  // -- Serial port -----------------------------------------------
  output        tx_o;
  input         rx_i;

  // -- Four hex digits -------------------------------------------
  output [15:0] display_o;

  // -- Cellular RAM, strobes active low --------------------------
  output [22:0] mem_adr_o;
  output        mem_cen_o;
  output        mem_oen_o;
  output        mem_wen_o;
  output        mem_ubn_o;
  output        mem_lbn_o;
  input [15:0]  mem_dat_i;
  output [15:0] mem_dat_o;
  output        mem_dat_oe_o;

  // Core/Wishbone interface
  wire [15:0]   wb2mx_dat;
  wire [15:0]   mx2wb_dat;
  wire [31:0]   mx2wb_adr;
  wire [1:0]    mx2wb_sel;
  wire          mx2wb_we;
  wire          mx2wb_cyc;
  wire          mx2wb_stb;
  wire          wb2mx_ack;
  wire          irq;

  // Boot ROM/Wishbone interface
  wire [15:0]   br2wb_dat;
  wire [31:0]   wb2br_adr;
  wire [1:0]    wb2br_sel;
  wire          wb2br_we;
  wire          wb2br_cyc;
  wire          wb2br_stb;
  wire          br2wb_ack;

  // Boot RAM/Wishbone interface
  wire [15:0]   wb2rm_dat;
  wire [15:0]   rm2wb_dat;
  wire [31:0]   wb2rm_adr;
  wire [1:0]    wb2rm_sel;
  wire          wb2rm_we;
  wire          wb2rm_cyc;
  wire          wb2rm_stb;
  wire          rm2wb_ack;

  // Cellular RAM/Wishbone interface
  wire [15:0]   wb2cr_dat;
  wire [15:0]   cr2wb_dat;
  wire [31:0]   wb2cr_adr;
  wire [1:0]    wb2cr_sel;
  wire          wb2cr_we;
  wire          wb2cr_cyc;
  wire          wb2cr_stb;
  wire          cr2wb_ack;

  // Peripherals/Wishbone interface
  wire [15:0]   wb2pe_dat;
  wire [15:0]   pe2wb_dat;
  wire [31:0]   wb2pe_adr;
  wire [1:0]    wb2pe_sel;
  wire          wb2pe_we;
  wire          wb2pe_cyc;
  wire          wb2pe_stb;
  wire          pe2wb_ack;

  wb_intercon #(.data_width (16),
		/* Boot ROM @ 0xFFFF0000 */
		.slave_0_mask (32'hFFFF_F000),
		.slave_0_addr (32'hFFFF_0000),
		/* Boot RAM @ 0xFFFF1000 */
		.slave_1_mask (32'hFFFF_F000),
		.slave_1_addr (32'hFFFF_1000),
		/* Cellular RAM - 16MB @ 0x00000000 */
		.slave_2_mask (32'hFF00_0000),
		.slave_2_addr (32'h0000_0000),
		/* Peripherals @ 0xF0000000 */
		.slave_3_mask (32'hFFFF_FE00),
		.slave_3_addr (32'hF000_0000),
		/* Unused */
		.slave_4_mask (32'h0000_0000),
		.slave_4_addr (32'hFFFF_FFFF),
		.slave_5_mask (32'h0000_0000),
		.slave_5_addr (32'hFFFF_FFFF),
		.slave_6_mask (32'h0000_0000),
		.slave_6_addr (32'hFFFF_FFFF))

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

		.wbs_1_dat_o (wb2rm_dat),
		.wbs_1_dat_i (rm2wb_dat),
		.wbs_1_adr_o (wb2rm_adr),
		.wbs_1_sel_o (wb2rm_sel),
		.wbs_1_we_o (wb2rm_we),
		.wbs_1_cyc_o (wb2rm_cyc),
		.wbs_1_stb_o (wb2rm_stb),
		.wbs_1_ack_i (rm2wb_ack),

		.wbs_2_dat_o (wb2cr_dat),
		.wbs_2_dat_i (cr2wb_dat),
		.wbs_2_adr_o (wb2cr_adr),
		.wbs_2_sel_o (wb2cr_sel),
		.wbs_2_we_o (wb2cr_we),
		.wbs_2_cyc_o (wb2cr_cyc),
		.wbs_2_stb_o (wb2cr_stb),
		.wbs_2_ack_i (cr2wb_ack),

		.wbs_3_dat_o (wb2pe_dat),
		.wbs_3_dat_i (pe2wb_dat),
		.wbs_3_adr_o (wb2pe_adr),
		.wbs_3_sel_o (wb2pe_sel),
		.wbs_3_we_o (wb2pe_we),
		.wbs_3_cyc_o (wb2pe_cyc),
		.wbs_3_stb_o (wb2pe_stb),
		.wbs_3_ack_i (pe2wb_ack),

		.wbs_4_dat_o (),
		.wbs_4_dat_i (16'h0000),
		.wbs_4_adr_o (),
		.wbs_4_sel_o (),
		.wbs_4_we_o (),
		.wbs_4_cyc_o (),
		.wbs_4_stb_o (),
		.wbs_4_ack_i (1'b0),

		.wbs_5_dat_o (),
		.wbs_5_dat_i (16'h0000),
		.wbs_5_adr_o (),
		.wbs_5_sel_o (),
		.wbs_5_we_o (),
		.wbs_5_cyc_o (),
		.wbs_5_stb_o (),
		.wbs_5_ack_i (1'b0),

		.wbs_6_dat_o (),
		.wbs_6_dat_i (16'h0000),
		.wbs_6_adr_o (),
		.wbs_6_sel_o (),
		.wbs_6_we_o (),
		.wbs_6_cyc_o (),
		.wbs_6_stb_o (),
		.wbs_6_ack_i (1'b0));

  moxie #(.ICACHE_SET_BITS (ICACHE_SET_BITS),
	  .BOOT_ADDRESS (32'hFFFF_0000)) core (.rst_i (rst_i),
					       .clk_i (clk_i),
					       .irq_i (irq),
					       .wb_dat_i (wb2mx_dat),
					       .wb_dat_o (mx2wb_dat),
					       .wb_adr_o (mx2wb_adr),
					       .wb_sel_o (mx2wb_sel),
					       .wb_we_o (mx2wb_we),
					       .wb_cyc_o (mx2wb_cyc),
					       .wb_stb_o (mx2wb_stb),
					       .wb_ack_i (wb2mx_ack));

  bootrom16 rom (.clk_i (clk_i),
		 .wb_dat_i (16'h0000),
		 .wb_dat_o (br2wb_dat),
		 .wb_adr_i (wb2br_adr),
		 .wb_sel_i (wb2br_sel),
		 .wb_we_i (wb2br_we),
		 .wb_tga_i (1'b0),
		 .wb_cyc_i (wb2br_cyc),
		 .wb_stb_i (wb2br_stb),
		 .wb_ack_o (br2wb_ack));

  ram16bit_wb ram (.clk_i (clk_i),
		   .rst_i (rst_i),
		   .wb_dat_i (wb2rm_dat),
		   .wb_dat_o (rm2wb_dat),
		   .wb_adr_i (wb2rm_adr),
		   .wb_sel_i (wb2rm_sel),
		   .wb_we_i (wb2rm_we),
		   .wb_cyc_i (wb2rm_cyc),
		   .wb_stb_i (wb2rm_stb),
		   .wb_ack_o (rm2wb_ack));

  psram_mox125_wb #(.STROBE_CLOCKS (RAM_STROBE_CLOCKS),
		    .ADDR_BITS (23)) cellram (.rst_i (rst_i),
					      .clk_i (clk_i),
					      .wb_dat_i (wb2cr_dat),
					      .wb_dat_o (cr2wb_dat),
					      .wb_adr_i (wb2cr_adr),
					      .wb_sel_i (wb2cr_sel),
					      .wb_we_i (wb2cr_we),
					      .wb_cyc_i (wb2cr_cyc),
					      .wb_stb_i (wb2cr_stb),
					      .wb_ack_o (cr2wb_ack),
					      .mem_adr_o (mem_adr_o),
					      .mem_cen_o (mem_cen_o),
					      .mem_oen_o (mem_oen_o),
					      .mem_wen_o (mem_wen_o),
					      .mem_ubn_o (mem_ubn_o),
					      .mem_lbn_o (mem_lbn_o),
					      .mem_dat_i (mem_dat_i),
					      .mem_dat_o (mem_dat_o),
					      .mem_dat_oe_o (mem_dat_oe_o));

  mox125_board_wb #(.UART_DIVISOR (UART_DIVISOR)) board (.rst_i (rst_i),
							 .clk_i (clk_i),
							 .wb_dat_i (wb2pe_dat),
							 .wb_dat_o (pe2wb_dat),
							 .wb_adr_i (wb2pe_adr),
							 .wb_sel_i (wb2pe_sel),
							 .wb_we_i (wb2pe_we),
							 .wb_cyc_i (wb2pe_cyc),
							 .wb_stb_i (wb2pe_stb),
							 .wb_ack_o (pe2wb_ack),
							 .irq_o (irq),
							 .rx_i (rx_i),
							 .tx_o (tx_o),
							 .display_o (display_o));

endmodule // nexys3_soc
