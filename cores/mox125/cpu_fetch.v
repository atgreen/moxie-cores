// cpu_fetch.v - The instruction fetch unit
//
// Copyright (c) 2010, 2011, 2012 Anthony Green.
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

// branch_flag_i - set high when we need to flush the pipeline and
//                 move to a new PC (branch_target_i).

module cpu_fetch #(parameter BOOT_ADDRESS = 32'h00001000
		   )(/*AUTOARG*/
   // Outputs
   imem_address_o, PC_o, opcode, valid, operand,
   // Inputs
   rst_i, clk_i, imem_data_i, branch_flag_i, branch_target_i, stall_i
   );
  
  // --- Clock and Reset ------------------------------------------
  input  rst_i, clk_i;
  
  // --- Instruction memory ---------------------------------------
  output [31:0] imem_address_o;
  input  [31:0] imem_data_i;
  output [31:0] PC_o;

  // --- Branch controls ------------------------------------------
  input 	branch_flag_i;
  input [31:0]	branch_target_i;
  
  // --- Pipeline interlock ---------------------------------------
  input stall_i;
  
  // --- Instruction Buffer ---------------------------------------
  output [15:0] opcode;
  output [0:0] 	valid;
  output [31:0] operand;

  // The program counter that we're using for fetching.
  wire [31:0] 	fetchPC;
  reg [31:0] 	nextPC;

  wire [0:0] 	valid, full, rden, wren;
  wire [0:0] 	newPC_p;
  wire [15:0]   opcode;
  wire [31:0]   operand;

  wire [31:0]  imem_address_o;
  
  assign imem_address_o = fetchPC;
  
  // The instruction FIFO
  cpu_ififo ififo (
		   // Outputs
		   .opcode_o	(opcode[15:0]),
		   .operand_o	(operand[31:0]),
		   .valid_o	(valid),
		   .full_o	(full),
		   .PC_o	(PC_o),
		   // Inputs
		   .rst_i	(rst_i),
		   .clk_i	(clk_i),
		   .PC_i	(fetchPC),
		   .newPC_p_i	(newPC_p),
		   .write_en_i	(wren),
		   .read_en_i	(rden),
		   .data_i	(imem_data_i));

  /* Only read if we aren't stalled or branching.  This makes sure we don't increment
     the PC in those cases.  */
  assign rden = !(stall_i | branch_flag_i);

  reg delay_write;

  /* Only write if we the instruction FIFO isn't full or we aren't branching.  */
  assign wren = !(rst_i | full | branch_flag_i | delay_write);

  assign newPC_p = rst_i | (!stall_i & branch_flag_i);

  assign fetchPC = rst_i ? BOOT_ADDRESS :
   		     branch_flag_i ? branch_target_i : nextPC;

  always @(posedge clk_i) begin
     nextPC <= wren ? fetchPC+4 : fetchPC;
     delay_write <= branch_flag_i;
  end

endmodule // cpu_fetch
