// psram_wb.v - Asynchronous PSRAM wishbone interface
//
// Copyright (c) 2013  Anthony Green
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

module psram_wb (/*AUTOARG*/
   // Outputs
   wb_dat_o, wb_ack_o, mem_adr_o, mem_clk_o, mem_cen_o, mem_cre_o,
   mem_oen_o, mem_wen_o, mem_adv_o, mem_ben_o,
   // Inouts
   mem_data_t,
   // Inputs
   rst_i, clk_i, wb_adr_i, wb_dat_i, wb_sel_i, wb_we_i, wb_cyc_i,
   wb_stb_i, mem_wait_i
   );

   input         rst_i,     clk_i;

   input [31:0]  wb_adr_i;
   output reg [15:0] wb_dat_o;
   input [15:0]      wb_dat_i;
   input [1:0] 	     wb_sel_i;
   input 	     wb_we_i;
   input 	     wb_cyc_i;
   input 	     wb_stb_i;
   output reg 	     wb_ack_o;

   // Memory interface
   output reg [22:0] mem_adr_o;   //Memory address
   output 	     mem_clk_o;    //Memory clock
   output reg 	     mem_cen_o;    //Memory chip enable
   output 	     mem_cre_o;    //Memory control register enable
   output reg 	     mem_oen_o;    //Memory output enable
   output reg 	     mem_wen_o;    //Memory write enable
   output 	     mem_adv_o;    //Memory address valid
   input 	     mem_wait_i;   //Memory wait
   output reg [ 1:0] mem_ben_o;     //Memory byte enable

   inout [15:0]      mem_data_t;  //Memory data tri-state

   assign mem_adv_o = 1'b0;   // Tie this low for async access.
   assign mem_clk_o = 1'b0;   // Ditto.
   assign mem_cre_o = 1'b0;   // Hold this low.

   reg [15:0] 	     a = 0;
   reg [15:0] 	     b = 0;
   reg [2:0] 	     wait_count = 0;
   reg 		     writing_now;

   // mem_data is tri-state when mem_oen is low.
   assign mem_data_t = mem_oen_o ? a : 16'bz;

   always @(posedge clk_i)
     begin
	b <= mem_data_t;
	a <= wb_dat_i;
     end

   wire finished_write = writing_now && (wait_count == 3);
   wire finished_read  = ~writing_now && (wait_count == 4);
   
   // --- State machine states -----------------------------------------
   parameter PSRAM_IDLE = 2'b00;
   parameter PSRAM_WAIT  = 2'b10;

   (* FSM_ENCODING="ONE-HOT", SAFE_IMPLEMENTATION="NO" *) reg [1:0] state = 0;

   always @(posedge clk_i) begin
      if (rst_i) begin
	 state <= PSRAM_IDLE;
	 wb_dat_o <= 0;
	 writing_now <= 0;
	 mem_cen_o <= 1'b1;
	 mem_oen_o <= 1'b1;
	 mem_wen_o <= 1'b1;
	 mem_adr_o <= 0;
	 mem_ben_o <= 2'b11;
      end
      else
	case (state)
	  PSRAM_IDLE: 
	    begin
	       if (wb_stb_i & wb_cyc_i)
		 state <= PSRAM_WAIT;
	       else
		 state <= PSRAM_IDLE;

	       if (wb_stb_i & wb_cyc_i)
		 begin
		    mem_adr_o <= wb_adr_i;
		    writing_now <= wb_we_i;
		    mem_oen_o <= wb_we_i;
		    mem_wen_o <= ~wb_we_i;
		    mem_cen_o <= 1'b0;
		    mem_ben_o <= wb_sel_i;
		 end
	       else
		 begin
		    mem_adr_o <= 26'hXXXXXXX;
		    writing_now <= 1'b0;
		    mem_oen_o <= 1'b1;
		    mem_wen_o <= 1'b1;
		    mem_cen_o <= 1'b1;
		    mem_ben_o <= 2'b11;
		 end
	    end

	  PSRAM_WAIT:
	    begin
	       // We need to wait 70ns for a write, and 70ns + 1 latch cycle for a read.
	       if (finished_read | finished_write)
		 state <= PSRAM_IDLE;
	       else
		 state <= PSRAM_WAIT;

	       if (finished_write)
		 begin
		    mem_oen_o <= 1'b1;
		    mem_wen_o <= 1'b1;
		    mem_cen_o <= 1'b1;
		    wait_count <= 0;
		    wb_ack_o <= 1'b1;
		 end
	       else if (finished_read)
		 begin
		    mem_oen_o <= 1'b1;
		    mem_wen_o <= 1'b1;
		    mem_cen_o <= 1'b1;
		    wait_count <= 0;
		    wb_ack_o <= 1'b1;
		    wb_dat_o <= b;
		 end
	       else
		 wait_count <= wait_count + 1;
	    end
	endcase
   end

endmodule
