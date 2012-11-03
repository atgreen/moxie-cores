// moxielite_wb.v - wishbone wrapper for moxielite.
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


module moxielite_wb(/*AUTOARG*/
   // Outputs
   wb_dat_o, wb_adr_o, wb_sel_o, wb_we_o, wb_cyc_o, wb_stb_o, debug_o,
   // Inputs
   rst_i, clk_i, wb_dat_i, wb_ack_i
   );

   // --- Clock and Reset ------------------------------------------
   input  rst_i, clk_i;
   
   // --- Wishbone Master Interface --------------------------------
   input [15:0]  wb_dat_i;
   output [15:0] wb_dat_o;
   output [31:0] wb_adr_o;
   output [1:0]  wb_sel_o;
   output        wb_we_o;
   output        wb_cyc_o;
   output        wb_stb_o;
   input         wb_ack_i;

   output [7:0]  debug_o;
   
   wire [31:1] 	 cpu_addr;
   wire [15:0] 	 cpu_din;
   wire [15:0] 	 cpu_dout;
   wire 	 cpu_rd_n; 
   wire 	 cpu_wr_n;
   wire 	 cpu_wr_h_n;
   wire 	 cpu_wr_l_n;
   wire 	 cpu_wr_h;
   wire 	 cpu_wr_l;

   reg [1:0] 	 cs; // current state
   reg [1:0] 	 ns; // next state

   localparam [1:0]
     IDLE = 2'b00,
     STRB = 2'b01,
     WAIT = 2'b11;

  reg 		 cpu_block = 1'b0;

   moxielite core (.clock (clk_i),
		   .reset_n (!rst_i),
		   .wait_n (!cpu_block),
		   .addr (cpu_addr),
		   .din (cpu_din),
		   .dout (cpu_dout),
		   .rd_n (cpu_rd_n),
		   .wr_n (cpu_wr_n),
		   .wr_h_n (cpu_wr_h_n),
		   .wr_l_n (cpu_wr_l_n),
		   .debug_o (debug_o));
   
   assign cpu_din = wb_dat_i;
   assign wb_dat_o = cpu_dout;
   assign wb_adr_o = {cpu_addr,1'b0};
   assign wb_we_o = ! cpu_wr_n;
   assign wb_cyc_o = wb_stb_o;
   assign wb_sel_o = cpu_wr_n ? 2'b11 : {!cpu_wr_h_n, !cpu_wr_l_n};

   wire strobe = !(cpu_rd_n & cpu_wr_n);

   assign wb_stb_o = (cs == STRB) ? strobe : 1'b0;
/*   
   always @(posedge clk_i)
     cs <= rst_i ? IDLE : ns;
*/
   reg debug_ack = 1'b0;
   reg [7:0] debug_data = 8'b0;
   
   always @(posedge clk_i) begin
     debug_ack <= debug_ack | wb_ack_i;
     debug_data <= debug_data | (wb_ack_i ? (wb_dat_i[15:8] | wb_dat_i[7:0]) : 16'b0);
   end;

//   assign debug_o = {cs, ns, cpu_rd_n, cpu_block, 2'b0};
   
   
  // cpu_block
  always @(*)
    case (cs)
      IDLE: cpu_block <= 1'b0;
      STRB: cpu_block <= 1'b1;
      WAIT: cpu_block <= !wb_ack_i;
    endcase

  // state machine
  // cs - current state
  always @(posedge clk_i)
    cs <= rst_i ? IDLE : ns;

  always @(*)
    case (cs)
      IDLE: ns <= wb_ack_i ? IDLE : (strobe ? STRB : IDLE);
      STRB: ns <= wb_ack_i ? IDLE : WAIT;
      WAIT: ns <= wb_ack_i ? IDLE : WAIT;
    endcase
  
endmodule