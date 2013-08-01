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

  reg 				  ack;
  
  assign wb_ack_o = ack;
   
  always @(posedge clk_i)
    begin
       ack <= wb_stb_i & wb_cyc_i;
    end

`ifdef XILINX
  ram4k16bit ram(.clka (clk_i),
		 .wea ({wb_stb_i & wb_cyc_i & wb_we_i, wb_stb_i & wb_cyc_i & wb_we_i}),
		 .addra (wb_adr_i[12:1]),
		 .dina (wb_dat_i),
		 .douta (wb_dat_o));
`else
  ram4k16bit ram(.clock (clk_i),
		 .wren ({wb_stb_i & wb_cyc_i & wb_we_i, wb_stb_i & wb_cyc_i & wb_we_i}),
		 .address (wb_adr_i[12:1]),
		 .data (wb_dat_i),
		 .q (wb_dat_o));
`endif

endmodule // ram16bit_wb
