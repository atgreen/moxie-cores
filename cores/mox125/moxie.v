// moxie.v - Top level Moxie Core
//
// Copyright (c) 2009, 2010, 2011, 2012  Anthony Green.
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

`include "defines.h"

module moxie (/*AUTOARG*/
   // Outputs
   wb_dat_o, wb_adr_o, wb_sel_i, wb_we_o, wb_cyc_o, wb_stb_o,
   wb_I_dat_o, wb_I_adr_o, wb_I_sel_o, wb_I_we_o, wb_I_cyc_o,
   wb_I_stb_o, wb_D_dat_o, wb_D_adr_o, wb_D_sel_o, wb_D_we_o,
   wb_D_cyc_o, wb_D_stb_o,
   // Inputs
   rst_i, clk_i, wb_dat_i, wb_ack_i, wb_I_dat_i, wb_I_ack_i,
   wb_D_dat_i, wb_D_ack_i
   );
   
  // --- Clock and Reset ------------------------------------------
  input  rst_i, clk_i;
  reg 	 rst;

  // --- Wishbone Interconnect ------------------------------------
  input [15:0]  wb_dat_i;
  output [15:0] wb_dat_o;
  output [31:0] wb_adr_o;
  output [1:0]   wb_sel_i;
  output        wb_we_o;
  output        wb_cyc_o;
  output        wb_stb_o;
  input         wb_ack_i;

  // --- Wishbone Interconnect for INSTRUCTION Memory -------------
  input [15:0]  wb_I_dat_i;
  output [15:0] wb_I_dat_o;
  output [31:0] wb_I_adr_o;
  output [1:0]   wb_I_sel_o;
  output        wb_I_we_o;
  output        wb_I_cyc_o;
  output        wb_I_stb_o;
  input         wb_I_ack_i;

 // --- Wishbone Interconnect for DATA Memory --------------------
  input [15:0]  wb_D_dat_i;
  output [15:0] wb_D_dat_o;
  output [31:0] wb_D_adr_o;
  output [1:0]   wb_D_sel_o;
  output        wb_D_we_o;
  output        wb_D_cyc_o;
  output        wb_D_stb_o;
  input         wb_D_ack_i;

  // --- Wishbone bus arbitration ---------------------------------
  assign wb_I_dat_i = wb_dat_i;
  assign wb_D_dat_i = wb_dat_i;
  assign wb_dat_o = wb_D_dat_o;
  assign wb_adr_o = wb_I_cyc_o ? wb_I_adr_o : wb_D_adr_o;
  assign wb_sel_o = wb_I_cyc_o ? 2'b11 : wb_D_sel_o;
  assign wb_we_o = wb_I_cyc_o ? 1'b0 : wb_D_we_o;
  assign wb_cyc_o = wb_I_cyc_o | wb_D_cyc_o;
  assign wb_stb_o = wb_I_cyc_o ? wb_I_stb_o : wb_D_stb_o;
  assign wb_I_ack_i  = wb_I_cyc_o ? wb_ack_i : 1'b0;
  assign wb_D_ack_i  = wb_I_cyc_o ? 1'b0 : wb_ack_i;

  // --- Wires to connect the 5 pipeline stages -------------------
  //
  //  Prefix codes for the control signals
  //        fd - Fetch to Decode
  //        dx - Decode to Execute
  //        rx - Register File to Execute
  //        xr - Execute to Register File

  wire [15:0] fd_opcode;
  wire [31:0] fd_operand;
  wire [31:0] fd_PC;
  wire [0:0]  fd_valid;
  wire [31:0] dx_operand;
  wire [31:0] dx_PC;
  wire [31:0] xw_PC;
  wire [`PCB_WIDTH-1:0] dx_pipeline_control_bits;
  wire [5:0]  dx_op;
  wire [9:0]  dx_pcrel_offset;
  wire [`PCB_WIDTH-1:0] xw_pipeline_control_bits;
  wire [0:0]  xr_register0_write_enable;
  wire [0:0]  xr_register1_write_enable;
  wire [3:0]  dx_register0_write_index;
  wire [3:0]  dx_register1_write_index;
  wire [3:0]  xr_register0_write_index;
  wire [3:0]  xr_register1_write_index;
  wire [31:0] xw_memory_address;
  wire [31:0] xr_reg0_result;
  wire [31:0] xr_reg1_result;
  wire [31:0] xw_mem_result;
  wire [3:0]  dx_regA;
  wire [3:0]  dx_regB;
  wire [3:0]  dx_regC;

  // Stack and frame pointers
  wire [31:0] rx_sp;
  wire [31:0] rx_fp;

  wire [0:0]  xf_branch_flag;
  wire [31:0] xf_branch_target;
 
  wire [31:0] rx_reg_value1;
  wire [31:0] rx_reg_value2;
  wire [3:0]  dr_reg_index1;
  wire [3:0]  dr_reg_index2;

  wire [0:0]  flush_x;

  // synthesis translate_off 
  initial
    begin
      $dumpvars(1,stage_fetch); 
      $dumpvars(1,stage_fetch.cache); 
      $dumpvars(1,stage_decode); 
      $dumpvars(1,stage_decode.mcode); 
      $dumpvars(1,stage_execute); 
      $dumpvars(1,stage_write);
      $dumpvars(1,stage_write.cache);
      $dumpvars(1,regs);
      $display("-- BEGINNING --");
    end
  // synthesis translate_on 

  cpu_registerfile regs (// Outputs
			 .value0_o (rx_reg_value1), 
			 .value1_o (rx_reg_value2),
			 // Inputs
			 .rst_i			(rst_i),
			 .clk_i			(clk_i),
			 .write_enable0_i (xr_register0_write_enable),
			 .write_enable1_i (xr_register1_write_enable), 
			 .reg_write_index0_i (xr_register0_write_index),
			 .reg_write_index1_i (xr_register1_write_index),
			 .reg_read_index0_i (dr_reg_index1), 
			 .reg_read_index1_i (dr_reg_index2),
			 .sp_o (rx_sp),
			 .fp_o (rx_fp),
			 .value0_i (xr_reg0_result),
			 .value1_i (0));

  // Forwarding logic.  
  reg maybe_forward_0;
  reg maybe_forward_1;
  wire forward_0 = maybe_forward_0 & dx_pipeline_control_bits[`PCB_RA];
  wire forward_1 = maybe_forward_1 & dx_pipeline_control_bits[`PCB_RB];
  
  cpu_fetch stage_fetch (// Outputs
			 .opcode		(fd_opcode[15:0]),
			 .valid		        (fd_valid),
			 .operand		(fd_operand[31:0]),
			 .imem_address_o        (wb_I_adr_o[31:0]),
			 .imem_stb_o            (wb_I_stb_o),
			 .imem_cyc_o            (wb_I_cyc_o),
			 .imem_ack_i            (wb_I_ack_i),
			 .PC_o                  (fd_PC[31:0]),
			 // Inputs
			 .rst_i			(rst_i),
			 .clk_i			(clk_i),
			 .branch_flag_i         (xf_branch_flag),
			 .branch_target_i       (xf_branch_target),
			 .stall_i               (1'b0),
			 .imem_data_i           (wb_I_dat_i[31:0]));
    
  cpu_decode stage_decode (// Inputs
			   .rst_i			(rst_i),
			   .clk_i			(clk_i),
			   .opcode_i		(fd_opcode[15:0]),
			   .operand_i		(fd_operand[31:0]),
			   .PC_i                (fd_PC[31:0]),
			   .valid_i		(fd_valid),
			   .flush_i             (flush_x),
			   .stall_i             (1'b0),
			   // Outputs
			   .pipeline_control_bits_o (dx_pipeline_control_bits),
			   .register0_write_index_o (dx_register0_write_index),
			   .register1_write_index_o (dx_register1_write_index),
			   .operand_o (dx_operand),
			   .PC_o (dx_PC),
			   .riA_o (dr_reg_index1),
			   .riB_o (dr_reg_index2),
			   .pcrel_offset_o (dx_pcrel_offset),
			   .op_o (dx_op));

  cpu_execute stage_execute (// Inputs
			     .rst_i	     (rst_i),
			     .clk_i	     (clk_i),
			     .flush_i        (1'b0),
			     .flush_o        (flush_x),
			     .op_i           (dx_op),
			     .PC_i           (dx_PC),
			     .PC_o           (xw_PC),
			     .pcrel_offset_i (dx_pcrel_offset),
			     .operand_i		(dx_operand[31:0]),
			     .regA_i (forward_0 ? xr_reg0_result : rx_reg_value1),
			     .regB_i (forward_1 ? xr_reg0_result : rx_reg_value2),
			     .branch_flag_o (xf_branch_flag),
			     .branch_target_o (xf_branch_target),
			     .pipeline_control_bits_i (dx_pipeline_control_bits),
			     .register0_write_index_i (dx_register0_write_index),
			     .register1_write_index_i (dx_register1_write_index),
			     // Outputs
			     .pipeline_control_bits_o (xw_pipeline_control_bits),
			     .register0_write_index_o (xr_register0_write_index),
			     .register1_write_index_o (xr_register1_write_index),
			     .reg0_result_o (xr_reg0_result),
			     .reg1_result_o (xr_reg1_result),
			     .mem_result_o (xw_mem_result),
			     .memory_address_o (xw_memory_address),
			     .sp_i (rx_sp),
			     .fp_i (rx_fp),
			     .register_wea_o (xr_register0_write_enable),
			     .register_web_o (xr_register1_write_enable));
  
  cpu_write stage_write (  // Inputs
			   .rst_i (rst_i),
			   .clk_i (clk_i),
			   .PC_i           (xw_PC),
			   .pipeline_control_bits_i (xw_pipeline_control_bits),
			   .memory_address_i (xw_memory_address),
			   .mem_result_i (xw_mem_result) );

  always @(posedge clk_i)
    begin
      // If we're writing to the same register we're about to read
      // from, then forward the value we're writing back into the
      // pipeline instead of reading from the register file.
      maybe_forward_0 <= xr_register0_write_enable
			 & (dx_register0_write_index == dr_reg_index1);
      maybe_forward_1 <= xr_register0_write_enable
			 & (dx_register0_write_index == dr_reg_index2);
    end

endmodule // moxie
