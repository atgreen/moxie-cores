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

module cpu_fetch #(parameter BOOT_ADDRESS = 32'h00001000
		   )(/*AUTOARG*/
  // Outputs
  imem_address_o, opcode, valid, operand, PC_o,
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
  reg [31:0] 	fetchPC;

  wire [0:0] 	valid, empty, full;
  reg 		wren, rden, flush_ififo;
  reg  [31:0] 	wrdata;
  reg  [0:0] 	newPC_p;
  wire [15:0]   opcode;
  wire [31:0]   operand;

  wire [31:0]  imem_address_o;
  
  assign imem_address_o = fetchPC;
  
  // The instruction FIFO
  cpu_ififo ififo (
		   // Outputs
		   .opcode_o		(opcode[15:0]),
		   .operand_o		(operand[31:0]),
		   .valid_o		(valid),
		   .empty_o		(empty),
		   .full_o		(full),
		   .PC_o                (PC_o),
		   // Inputs
		   .rst_i		(rst_i | flush_ififo),
		   .clk_i		(clk_i),
		   .stall_i             (stall_i),
		   .PC_i                (fetchPC),
		   .newPC_p_i           (newPC_p),
		   .write_en_i		(wren),
		   .read_en_i		(rden),
		   .data_i	(wrdata[31:0]));
  
  always @(posedge clk_i) begin
    if (rst_i) begin
      fetchPC <= #1 BOOT_ADDRESS;
      newPC_p <= #1 1;
    end else begin 
       if (! stall_i) begin
	 if (branch_flag_i) begin
	   fetchPC <= branch_target_i;
	   newPC_p <= 1;
	   wren <= 0;
	   rden <= 0;
	   flush_ififo <= 1;
	 end else begin
	   newPC_p <= 0;
	   flush_ififo <= 0;
	   if (! full) begin
	     wren <= 1;
	     wrdata <= imem_data_i;
	     fetchPC <= fetchPC+4;
	   end else begin
	     wren <= 0;
	   end
	   rden <= 1;
	 end
       end else begin
	 flush_ififo <= 0;
	 wren <= 0;
	 rden <= 0;
       end      
    end // else: !if(rst_i)
  end
  
endmodule // cpu_fetch
