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

  // 16k RAM
  reg  [7:0] ram[0:16383];
  wire [11:0] index;

  assign index = wb_adr_i[11:0];

  reg [0:0]  wb_ack_o_reg;
  reg [15:0] wb_dat_o_reg;

   assign wb_ack_o = wb_ack_o_reg;
   assign wb_dat_o = wb_dat_o_reg;
   
  always @(posedge clk_i)
    begin
      if (wb_sel_i[0])
	wb_dat_o_reg[15:8] <= ram[index];
      if (wb_sel_i[1])
	wb_dat_o_reg[7:0] <= ram[index+1];
      wb_ack_o_reg <= wb_stb_i & wb_cyc_i;
    end
  

   always @(posedge clk_i)
     begin
	if (wb_stb_i & wb_cyc_i & wb_we_i)
	  begin
	     if (wb_sel_i[1])
	       ram[index+1] <= wb_dat_i[7:0];
	     if (wb_sel_i[0])
	       ram[index] <= wb_dat_i[15:8];
	  end
     end
   
/*  ram4k16bit ram(.clock (clk_i),
		 .wren (!(wb_stb_i & wb_cyc_i & wb_we_i)),
		 .address (wb_adr_i[12:1]),
		 .data (wb_dat_i),
		 .q (wb_dat_o)); */
`endif

endmodule // ram16bit_wb
