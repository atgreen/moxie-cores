// cpu_registerfile.v - moxie register file
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

module cpu_registerfile (/*AUTOARG*/
  // Outputs
  value0_o, value1_o, sp_o, fp_o,
  // Inputs
  rst_i, clk_i, write_enable0_i, write_enable1_i, value0_i, value1_i,
  reg_write_index0_i, reg_write_index1_i, reg_read_index0_i,
  reg_read_index1_i
  );
   
  // synthesis translate_off 
  initial
    begin
       $dumpvars(1, mem_2w4r);
    end
  // synthesis translate_on 

  // --- Clock and Reset ------------------------------------------
  input  rst_i, clk_i;

   output [31:0] value0_o, value1_o;
   output [31:0] sp_o;
   output [31:0] fp_o;

   input write_enable0_i, write_enable1_i;
   input [31:0] value0_i, value1_i;
   input [0:3] reg_write_index0_i, reg_write_index1_i;
   input [0:3] reg_read_index0_i, reg_read_index1_i;

  MEM_2w4r mem_2w4r (.clock(clk_i),
		     .we0(write_enable0_i),
		     .we1(write_enable1_i),
		     .write_addr_0(reg_write_index0_i),
		     .write_data_0(value0_i),
		     .write_addr_1(reg_write_index1_i),
		     .write_data_1(value1_i),
		     .read_addr_0(reg_read_index0_i),
		     .read_data_0(value0_o),
		     .read_addr_1(reg_read_index1_i),
		     .read_data_1(value1_o),
		     .read_addr_2(4'b0),
		     .read_data_2(fp_o),
		     .read_addr_3(4'b1),
		     .read_data_3(sp_o));
  
endmodule // cpu_registerfile
