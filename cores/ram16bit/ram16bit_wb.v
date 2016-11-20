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

 
   lpm_ram_dq ram (.data(wb_dat_i), 
		   .address(wb_adr_i[12:1]), 
		   .we(!(wb_stb_i & wb_cyc_i & wb_we_i)),
		   .inclock(clk_i), 
                   .q(wb_dat_o));

// passing the parameter values
   defparam ram.lpm_width = 16;
   defparam ram.lpm_widthad = 8;
   defparam ram.lpm_indata = "REGISTERED";
   defparam ram.lpm_outdata = "REGISTERED";

`endif

endmodule // ram16bit_wb
