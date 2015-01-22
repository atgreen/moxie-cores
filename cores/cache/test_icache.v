module test_icache;

   reg rst, clk;

   reg [31:0] addr;
   reg stb;
   reg [15:0] mem_data;
   reg wb_ack;

   wire hit;
   wire [15:0] inst;
   wire [31:0] data;
   wire [31:0] wb_adr;
   wire [1:0]  wb_sel;
   wire        wb_cyc;
   wire        wb_stb;
      
   icache cache (.rst_i (rst), 
		 .clk_i (clk), 
		 .stb_i (stb),
		 .adr_i (addr),
		 .wb_dat_i (mem_data),
		 .hit_o (hit),
		 .inst_o (inst),
		 .data_o (data),
		 .wb_adr_o (wb_adr),
		 .wb_sel_o (wb_sel),
		 .wb_cyc_o (wb_cyc),
		 .wb_stb_o (wb_stb),
		 .wb_ack_i (wb_ack));
   
   initial begin
      $dumpvars (1, test_icache);
      $dumpvars (1, cache);

      clk = 0;
      rst = 0;
      addr = 0;
      stb = 1;
      wb_ack = 0;
      mem_data = 0;
   end

   always @(posedge clk)
     begin
	wb_ack <= wb_stb;
	mem_data <= wb_adr[15:0];
     end

   reg [15:0] hold_inst;
   reg [31:0] hold_data;
   
   // --- State machine states -----------------------------------------
   parameter ICACHE_REQ = 1'd0;
   parameter ICACHE_WAIT = 1'd1;

   reg 	      state = ICACHE_REQ;
   
   always @(posedge clk)
     begin
	if (rst) begin
	   addr <= 30;
	   wb_ack <= 0;
	   stb <= 0;
	   state <= ICACHE_REQ;
	end else
	  case (state)
	    ICACHE_REQ:
	      begin
	      	 stb <= 1;
	      	 state <= ICACHE_WAIT;
	      end
	    ICACHE_WAIT:
	      if (hit) begin
	      	 hold_inst <= inst;
	      	 hold_data <= data;
	      	 addr <= addr + 6;
	      	 state <= ICACHE_REQ;
	      	 stb <= 0;
	      end
	  endcase
     end
   
   initial
     begin
	#10 rst = 1;
	#20 rst = 0;
	#10000 $finish;
     end
   
/* verilator lint_off STMTDLY */
   always
     #5 clk = !clk;
/* verilator lint_on STMTDLY */
   
endmodule // test_icache
