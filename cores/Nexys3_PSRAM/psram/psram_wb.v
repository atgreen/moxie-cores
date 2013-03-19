module psram_wb (input         rst_i,
		 input 	       clk_i,
		 input [31:0]  wb_adr_i,
		 output [15:0] wb_dat_o,
		 input [15:0]  wb_dat_i,
		 input [1:0]   wb_sel_i,
		 input 	       wb_we_i,
		 input 	       wb_cyc_i,
		 input 	       wb_stb_i,
		 output        wb_ack_o, 

		 //Memory interface
		 output [22:0] mem_addr,   //Memory address
		 output        mem_clk,    //Memory clock
		 output        mem_cen,    //Memory chip enable
		 output        mem_cre,    //Memory control register enable
		 output        mem_oen,    //Memory output enable
		 output        mem_wen,    //Memory write enable
		 output        mem_adv,    //Memory address valid
		 input         mem_wait,   //Memory wait
		 output [ 1:0] mem_be,     //Memory byte enable

		 input  [15:0] mem_data_i, //Memory data in
		 output [15:0] mem_data_o, //Memory data out
		 output [15:0] mem_data_t  //Memory data tri-state
		 );

   wire 		       contoller_ready_;
   wire                        fml_eack_;
   wire [63:0] 		       fml_do_;
   
   psram_sync ps (.clk (clk_i),
		  .rst_n (!rst_i),
		  .controller_ready (controller_ready_),

		  .fml_adr (wb_adr_i[22:0]),
		  .fml_stb (wb_stb_i & wb_cyc_i),
		  .fml_we (wb_stb_i & wb_cyc_i & wb_we_i),
		  .fml_eack (fml_eack_),
		  .fml_sel (8'hFF),
		  .fml_di ({48'b0,wb_dat_i}),
		  .fml_do (fml_do_),

		  .mem_addr (mem_addr),
		  .mem_clk (mem_clk),
		  .mem_cen (mem_cen),
		  .mem_cre (mem_cre),
		  .mem_oen (mem_oen),
		  .mem_wen (mem_wen),
		  .mem_adv (mem_adv),
		  .mem_wait (mem_wait),
		  .mem_be (mem_be),

		  .mem_data_i (mem_data_i),
		  .mem_data_o (mem_data_o),
		  .mem_data_t (mem_data_t));

endmodule // psram_wb
