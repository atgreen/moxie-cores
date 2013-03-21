module mpic_wb (/*AUTOARG*/
   // Outputs
   wb_dat_o, wb_ack_o, irq_o,
   // Inputs
   rst_i, clk_i, wb_dat_i, wb_sel_i, wb_we_i, wb_cyc_i, wb_stb_i,
   irq_i
   );

   input  rst_i, clk_i;
   input [15:0] wb_dat_i;
   output [15:0] wb_dat_o;
   input [1:0] 	wb_sel_i;
   input 	wb_we_i;
   input 	wb_cyc_i;
   input 	wb_stb_i;
   output reg	wb_ack_o;
   input [4:0] 	irq_i;
   output reg	irq_o;

   reg [4:0] 	irq;
   
   wire 	we;

   assign we = wb_stb_i & wb_cyc_i & wb_we_i;

   assign wb_dat_o = {11'b0, irq};
   
   always @(posedge clk_i)
     irq_o <= irq[0] | irq[1] | irq[2] | irq[3] | irq[4];

   always @(posedge clk_i)
     irq[0] <= (rst_i | (we & wb_dat_i[0])) ? 1'b0 : irq[0] | irq_i[0];
   always @(posedge clk_i)
     irq[1] <= (rst_i | (we & wb_dat_i[1])) ? 1'b0 : irq[1] | irq_i[1];
   always @(posedge clk_i)
     irq[2] <= (rst_i | (we & wb_dat_i[2])) ? 1'b0 : irq[2] | irq_i[2];
   always @(posedge clk_i)
     irq[3] <= (rst_i | (we & wb_dat_i[3])) ? 1'b0 : irq[3] | irq_i[3];
   always @(posedge clk_i)
     irq[4] <= (rst_i | (we & wb_dat_i[4])) ? 1'b0 : irq[4] | irq_i[4];
   
  always @(posedge clk_i)
    begin
      wb_ack_o <= wb_stb_i & wb_cyc_i;
    end

endmodule // mpic_wb
