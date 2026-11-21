`include "config.v"

module ram16bit_wb (input         rst_i,
		    input 	  clk_i,
		    input [31:0]  wb_adr_i,
		    output [15:0] wb_dat_o,
		    input [15:0]  wb_dat_i,
		    input [1:0]   wb_sel_i,
		    input 	  wb_we_i,
		    input 	  wb_cyc_i,
		    input 	  wb_stb_i,
		    output 	  wb_ack_o);

`ifdef XILINX

  reg 				  ack;

  assign wb_ack_o = ack;

  always @(posedge clk_i)
    begin
       ack <= wb_stb_i & wb_cyc_i;
    end

  ram4k16bit ram(.clka (clk_i),
		 .wea ({wb_stb_i & wb_cyc_i & wb_we_i, wb_stb_i & wb_cyc_i & wb_we_i}),
		 .addra (wb_adr_i[12:1]),
		 .dina (wb_dat_i),
		 .douta (wb_dat_o));
`else

  // 4k boot RAM, as two memories a byte wide: the bytes at even
  // addresses and the bytes at odd ones.  Each is read and written at
  // one address with a registered read, which is the shape block RAM
  // has, so this does not turn into thirty thousand flip-flops.
  reg [7:0] 			  ram_even[0:2047];
  reg [7:0] 			  ram_odd[0:2047];

  reg [7:0] 			  q_even, q_odd;
  reg 				  odd_q;
  reg 				  ack;

  wire [10:0] 			  index = wb_adr_i[11:1];
  wire 				  odd = wb_adr_i[0];
  wire 				  write = wb_stb_i & wb_cyc_i & wb_we_i;

  // The core's bus convention: a word is the byte at the address and
  // the one after it.  A byte is read on the high lane, and written
  // from the low lane with only the low select set.
  wire 				  byte_write = (wb_sel_i == 2'b01);

  assign wb_ack_o = ack;
  assign wb_dat_o = odd_q ? {q_odd, q_even} : {q_even, q_odd};

  always @(posedge clk_i)
    begin
      ack <= wb_stb_i & wb_cyc_i;
      odd_q <= odd;
    end

  always @(posedge clk_i)
    begin
      q_even <= ram_even[index];
      if (write & (byte_write ? !odd : 1'b1))
	ram_even[index] <= byte_write ? wb_dat_i[7:0] : wb_dat_i[15:8];
    end

  always @(posedge clk_i)
    begin
      q_odd <= ram_odd[index];
      if (write & (byte_write ? odd : 1'b1))
	ram_odd[index] <= wb_dat_i[7:0];
    end

`endif

endmodule // ram16bit_wb
