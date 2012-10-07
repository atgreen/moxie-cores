// cpu_ififo.v - The instruction FIFO unit
//
// Copyright (c) 2010, 2011, 2012 Anthony Green.  All Rights Reserved.
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

module cpu_ififo #(parameter BOOT_ADDRESS = 32'h00001000
		   )(/*AUTOARG*/
   // Outputs
   PC_o, opcode_o, operand_o, valid_o, full_o,
   // Inputs
   rst_i, clk_i, write_en_i, read_en_i, data_i, newPC_p_i, PC_i
   );
  
  // --- Clock and Reset ------------------------------------------
  input  rst_i, clk_i;

  input write_en_i ; // assert high to write data
  input read_en_i ; // assert high to read data

  input [31:0] data_i ; // data to write

  input [0:0]  newPC_p_i;
  input [31:0] PC_i;
  wire [31:0]   PC;
  reg [31:0]   next_PC;
  output [31:0] PC_o;
  reg [31:0] 	PC_o;
  
  output [15:0] opcode_o; // opcode_o for newly read instruction
  output [31:0] operand_o; // data operand_o for newly read instruction
  output [0:0]	valid_o;
  output [0:0]	full_o ;
  
  reg [1:0]    read_ptr, write_ptr; // write and read pointers
  reg [2:0]    ptr_gap; // gap between the pointers
  reg [15:0]   opcode_o;
  reg [31:0]   operand_o;
  reg [0:0]    valid_o;
  reg [0:0]    full_o;
  reg [15:0]   buffer[3:0]; // instruction buffer
  
  wire [0:0]   can_write_32, can_read_16, can_read_48;
  wire [0:0]   buffer_full,  buffer_empty;

  assign can_write_32 = ((ptr_gap == 0) || (ptr_gap == 1) || (ptr_gap == 2));
  assign can_read_16 = (ptr_gap != 0);
  assign can_read_48 =  ((ptr_gap != 0) && (ptr_gap != 1) && (ptr_gap != 2));
  assign buffer_full = ptr_gap == 4;
  assign buffer_empty = ptr_gap == 0;

  // This is a rediculous bit of logic.  We should try to recode the
  // moxie instructions so that we can determine the length with less
  // logic.
  function [0:0] is_long_insn;
    input [7:0] op;
    is_long_insn = ((op == 8'h01) //  ldi.l
		    || (op == 8'h03) // jsra
		    || (op == 8'h08) // lda.l
		    || (op == 8'h09) // sta.l
		    || (op == 8'h0c) // ldo.l
		    || (op == 8'h0d) // sto.l
		    || (op == 8'h1a) // jmpa
		    || (op == 8'h1b) // ldi.b
		    || (op == 8'h1d) // lda.b
		    || (op == 8'h1f) // sta.b
		    || (op == 8'h20) // ldi.s
		    || (op == 8'h22) // lda.s
		    || (op == 8'h24) // sta.s
		    || (op == 8'h25) // jmp
		    || (op == 8'h30) // swi
		    || (op == 8'h36) // ldo.b
		    || (op == 8'h37) // sto.b
		    || (op == 8'h38) // ldo.s
		    || (op == 8'h39)); // sto.s
  endfunction

  assign PC = (newPC_p_i ? PC_i : next_PC);

  always @(posedge clk_i)
    if (rst_i | newPC_p_i) begin
      opcode_o <= 16'b0;
      operand_o <= 32'b0;
      read_ptr <= 0;
      write_ptr <= 0;
      ptr_gap = 0;
      full_o = 0;
      valid_o <= 0;
      next_PC <= PC_i;
    end else begin
       PC_o <= PC;
       // $display ("A %x buffer[read_ptr] = 0x%x", read_ptr, buffer[read_ptr][15:8]);
       // $display ("A BUFFER = 0x%x%x%x%x", buffer[0], buffer[1], buffer[2], buffer[3]);
       // $display ("A buffer_empty = %x", buffer_empty);
       // $display ("A ptr_gap = 0x%x", ptr_gap);
       // $display ("A 0x%x", !is_long_insn(buffer[read_ptr][15:8]));
       // $display ("A 0x%x", buffer[read_ptr][15:8]);
       if (buffer_empty ? !is_long_insn(data_i[31:24]) : !is_long_insn(buffer[read_ptr][15:8])) begin
	  // This is a 16-bit instruction (either from input or from buffer).
	  //	$display ("B");
	  if (write_en_i && (!read_en_i) && (can_write_32)) begin
	     //	  $display ("Z");
	     buffer[write_ptr] <= data_i[31:16];
	     buffer[(write_ptr+1)%4] <= data_i[15:0];
	     write_ptr <= (write_ptr + 2) % 4;
	     valid_o <= 0;
	     ptr_gap <= ptr_gap + 2;
	     full_o <= ((ptr_gap == 1) || (ptr_gap == 2));
	  end
	  else if ((!write_en_i) && read_en_i && (can_read_16)) begin
	     // $display ("Y");
	     opcode_o <= buffer[read_ptr];
	     valid_o <= 1;
	     next_PC <= PC + 2;
	     read_ptr <= (read_ptr + 1) % 4;
	     ptr_gap <= ptr_gap - 1;
	     full_o <= 1'b0;
	  end
	  else if (write_en_i && read_en_i && buffer_empty) begin
	     opcode_o <= data_i[31:16];
	     buffer[0] <= data_i[15:0];
	     write_ptr <= 1;
	     read_ptr <= 0;
	     ptr_gap <= 1;
	     full_o <= 1'b0;
	     valid_o <= 1'b1;
	     next_PC <= PC + 2;
	  end
	  else if (write_en_i && read_en_i && buffer_full) begin
	     opcode_o <= buffer[read_ptr];
	     valid_o <= 1;
	     next_PC <= PC + 2;
	     read_ptr <= (read_ptr + 1) % 4;
	     ptr_gap <= ptr_gap - 1;
	     full_o <= 0;
	  end
	  else if (write_en_i && read_en_i && (can_write_32) && (can_read_16)) begin
	     buffer[write_ptr] <= data_i[31:16];
	     buffer[(write_ptr+1)%4] <= data_i[15:0];
	     write_ptr <= (write_ptr + 2) % 4;
	     opcode_o <= buffer[read_ptr];
	     read_ptr <= (read_ptr + 1) % 4;
	     valid_o <= 1;
	     next_PC <= PC + 2;
	     ptr_gap = ptr_gap + 1;
	     full_o = ((ptr_gap == 3) || (ptr_gap == 4));
	  end 
	  else begin
	     full_o = ((ptr_gap == 3) || (ptr_gap == 4));
	     // #1 $display ("U - ERROR %x %x %x %x %x %x", write_en_i, read_en_i, write_ptr, read_ptr, ptr
	     // _gap, full_o);
	  end
       end else begin
	  if (write_en_i && (!read_en_i) && (can_write_32)) begin
	     buffer[write_ptr] <= data_i[31:16];
	     buffer[(write_ptr+1)%4] <= data_i[15:0];
	     write_ptr <= write_ptr + 2;
	     valid_o <= 0;
	     ptr_gap = ptr_gap + 2;
	     full_o = ((ptr_gap == 3) || (ptr_gap == 4));
	  end
	  else if ((!write_en_i) && read_en_i && (can_read_48)) begin
	     opcode_o <= buffer[read_ptr];
	     operand_o[31:16] <= buffer[(read_ptr+1)%4];
	     operand_o[15:0] <= buffer[(read_ptr+2)%4];
	     valid_o <= 1;
	     next_PC <= PC + 6;
	     read_ptr <= read_ptr + 3;
	     ptr_gap = ptr_gap - 3;
	     full_o = ((ptr_gap == 3) || (ptr_gap == 4));
	  end
	  else if (write_en_i && read_en_i && buffer_empty) begin
	     buffer[write_ptr] <= data_i[31:16];
	     buffer[write_ptr+1] <= data_i[15:0];
	     write_ptr <= write_ptr + 2;
	     valid_o <= 0;
	     ptr_gap = ptr_gap + 2;
	     full_o = ((ptr_gap == 3) || (ptr_gap == 4));
	  end
	  else if (write_en_i && read_en_i && buffer_full) begin
	     opcode_o <= buffer[read_ptr];
	     operand_o[31:16] <= buffer[read_ptr+1];
	     operand_o[15:0] <= buffer[read_ptr+2];
	     valid_o <= 1;
	     next_PC <= PC + 6;
	     read_ptr <= read_ptr + 3;
	     ptr_gap = ptr_gap - 3;
	     full_o = ((ptr_gap == 3) || (ptr_gap == 4));
	  end
	  else if (write_en_i && read_en_i && (can_write_32) && (can_read_48)) begin
	     buffer[write_ptr] <= data_i[31:16];
	     buffer[write_ptr+1] <= data_i[15:0];
	     write_ptr <= write_ptr + 2;
	     opcode_o <= buffer[read_ptr];
	     operand_o[31:16] <= buffer[read_ptr+1];
	     operand_o[15:0] <= buffer[read_ptr+2];
	     read_ptr <= read_ptr + 3;
	     valid_o <= 1;
	     next_PC <= PC + 6;
	     ptr_gap = ptr_gap - 1;
	     full_o = ((ptr_gap == 3) || (ptr_gap == 4));
	  end
	  else if (write_en_i && read_en_i && (ptr_gap == 2)) begin
	     buffer[(write_ptr)%4] <= data_i[31:16];
	     buffer[(write_ptr+1)%4] <= data_i[15:0];
	     write_ptr <= (write_ptr + 2) % 4;
	     opcode_o <= buffer[read_ptr];
	     operand_o[31:16] <= buffer[(read_ptr+1)%4];
	     operand_o[15:0] <= data_i[31:16];
	     read_ptr <= (read_ptr + 3) % 4;
	     valid_o <= 1;
	     next_PC <= PC + 6;
	     ptr_gap = 1;
	     full_o = ((ptr_gap == 3) || (ptr_gap == 4));
	  end
	  else if (write_en_i && read_en_i && (ptr_gap == 1)) begin
	     opcode_o <= buffer[read_ptr];
	     operand_o[31:16] <= data_i[31:16];
	     operand_o[15:0] <= data_i[15:0];
	     read_ptr <= (read_ptr + 1) % 4; // FIXME: this is probably not needed
	     valid_o <= 1;
	     next_PC <= PC + 6;
	     ptr_gap = 0;
	     full_o = ((ptr_gap == 3) || (ptr_gap == 4)); // FIXME: neither is this
	  end 
	  else if (write_en_i && read_en_i && (ptr_gap == 3)) begin
	     buffer[write_ptr] <= data_i[31:16];
	     buffer[write_ptr+1] <= data_i[15:0];
	     write_ptr <= write_ptr + 2;
	     opcode_o <= buffer[read_ptr];
	     operand_o[31:16] <= buffer[(read_ptr+1)%4];
	     operand_o[15:0] <= buffer[(read_ptr+2)%4];
	     read_ptr <= read_ptr + 3;
	     valid_o <= 1;
	     next_PC <= PC + 6;
	     ptr_gap = ptr_gap - 1;
	     full_o = ((ptr_gap == 3) || (ptr_gap == 4)); // FIXME: this is probably not needed
	  end
       end
    end
endmodule

