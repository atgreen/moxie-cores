// wb_watchdog.v - Wishbone Shared Bus Watchdog
//
// Copyright (c) 2012  Anthony Green.
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


module wb_watchdog #(
  parameter timeout_width = 4
  )(/*AUTOARG*/
  // Outputs
  adr_o, fault_o,
  // Inputs
  clk_i, rst_i, wbm_adr_i, wbm_stb_i, wbm_ack_i
  );

  // Wishbone Master Interface
  input         clk_i;
  input 	rst_i;
  input [31:0]  wbm_adr_i;
  input         wbm_stb_i;
  input 	wbm_ack_i;
  output        adr_o;
  output        fault_o;

  // Watchdog counter
  reg [timeout_width-1:0] counter = 0;

  // Address
  reg [31:0] 	address;

  // Watchdog active flag
  reg 		active = 1'b0;

  wire 		timeout;
  assign timeout = (counter == 4'b1111);
  
  assign adr_o = address;
  assign fault_o = active & timeout;

  always @(posedge clk_i) begin
    counter <= wbm_stb_i ? 0 : counter + 1;
    address <= wbm_stb_i ? wbm_adr_i : address;
//    active <= (rst_i | fault_o) ? 1'b0 : wbm_stb_i ? 1'b1 : wbm_ack_i ? 1'b0 : active;
    active <= (rst_i) ? 1'b0 : wbm_stb_i ? 1'b1 : wbm_ack_i ? 1'b0 : active;
  end
    
endmodule // wb_watchdog

