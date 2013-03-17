
module top (  input clk,
    input ir0,
    input ir1,
    input ir2,
    input ir3,
    input ir4,
    input ir5,
    input ir6,
    input rst,
    input wb_cyc,
    input wb_sel,
    input wb_std,
    input wb_we,
    input [7:0] wb_addr,
    input [7:0] wb_din,
    output wb_ack,
    output wb_irq,
    output [7:0] wb_dout
    );
    
    wire [2:0] irq_no;
    wire [7:0] w_mask;
    wire [2:0] w_pir;
    
    pri_rslv1 U1
              (
                  .int_o(wb_irq),
                  .ir0(ir0),
                  .ir1(ir1),
                  .ir2(ir2),
                  .ir3(ir3),
                  .ir4(ir4),
                  .ir5(ir5),
                  .ir6(ir6),
                  .ir7(ir6),
                  .irq_no(irq_no),
                  .mask_in(w_mask),
                  .pri_in(w_pir)
              );

    wb_if U2
          (
              .clk(clk),
              .irq_no(irq_no),
              .mask(w_mask),
              .pri_no_reg(w_pir),
              .ret(rst),
              .wb_ack(wb_ack),
              .wb_addr(wb_addr[7:0]),
              .wb_cyc(wb_cyc),
              .wb_din(wb_din),
              .wb_dout(wb_dout),
              .wb_sel(wb_sel),
              .wb_std(wb_std),
              .wb_we(wb_we)
          );

endmodule


`define ADDR_MASK_REG 0
`define ADDR_PRI_NO_REG 1
`define ADDR_IRQ_NO_REG 2
`define WB_B2
//`define WB_B3

module wb_if(
        input clk,
        input rst,

        input [7:0] wb_din,
        output reg [7:0] wb_dout,
        input wb_sel,
        input wb_std,
        input wb_we,
        input wb_cyc,
        input [1:0] wb_addr,
        output reg wb_ack,

        input  [2:0] irq_no,
        output reg [7:0] mask,
        output reg [2:0] pri_no_reg
    );

    /*
    latch the irq_no input 
    */
    reg [2:0] irq_no_reg ;
    always@(posedge clk)   
		if (ret)irq_no_reg<=3'bxxx;
        irq_no_reg<=irq_no;

    reg [7:0] mask_reg ;
	/* the bit bit 0 is the mask of ir0
	for exmple the bit 0 is the mask of ir0.
	so when you clear bit 0 ,the ir0 interrupt request will be ignored 
	*/
    
    assign wb_access = wb_sel & wb_std & wb_cyc ;						
	
    assign sel_mask = wb_addr == `ADDR_MASK_REG && wb_access ;
    assign sel_irq_no_reg = wb_addr == `ADDR_IRQ_NO_REG&& wb_access ;
    assign sel_pri_no_reg = wb_addr == `ADDR_PRI_NO_REG && wb_access ;
	/*
	
	*/

`ifdef WB_B2
		always@(posedge clk)//WISHBONE B2        
`else
		always@(*) //WISHBONE B3
`endif
	  wb_ack = wb_access ;

    assign wr_mask = sel_mask & wb_we ;
    assign wr_irq_no_reg = sel_irq_no_reg & wb_we ;
    assign wr_pri_no_reg = sel_pri_no_reg& wb_we ;

    always@(posedge clk)
    case({sel_mask,sel_irq_no_reg,sel_pri_no_reg})
        1: wb_dout<={5'bxxxxx,pri_no_reg[2:0]} ; /*read the pripority number setted by WISHHONE bus before */
        2: wb_dout<={5'bxxxxx,irq_no_reg[2:0]};	 /*read which line has a interrupt request*/
        4: wb_dout<=mask_reg;/*read mask setted by WISHHONE bus before*/
        default :
            wb_dout<=8'bxxxx_xxxx; 	/*WISHBONE bus does not want to read this device after all and will ignore the output_data*/
    endcase

    always@(posedge clk)
    if (rst)begin 
    	mask_reg<=0;//disable all interrput
    	pri_no_reg <=3'bxxx;
    end else
    case ({wr_mask,wr_pri_no_reg})
        2:mask_reg <= wb_din; //write mask register
        1:pri_no_reg <= wb_din[2:0]; //write priory number register
    endcase
endmodule


module pri_rslv1(
        input ir0,
        input ir1,
        input ir2,
        input ir3,
        input ir4,
        input ir5,
        input ir6,
        input ir7,
        input [7:0] mask_in,
        input [2:0] pri_in,
        output reg [2:0] irq_no,
        output reg int_o
    );
    
    wire [7:0] valid_ir_data= {ir7,ir6,ir5,ir4,ir3,ir2,ir1,ir0} & mask_in;
    /*mask here used to disable some interrupt request which we do not care about*/
    
    reg [3:0] int_pri_ir;
    /*to indicate  priority of the current legal interruput*/
    
    always @(*)
    casex( valid_ir_data )
        /*
        8-3 Line Priority Encoder which is similar with 74LS148 device
        the ir0 has the highest pripority while the ir7 has the lowwest one
        */
        {8'bxxxxxxx1}: int_pri_ir = 7;
        {8'bxxxxxx10}: int_pri_ir = 6;
        {8'bxxxxx100}: int_pri_ir = 5;
        {8'bxxxx1000}: int_pri_ir = 4;
        {8'bxxx10000}: int_pri_ir = 3;
        {8'bxx100000}: int_pri_ir = 2;
        {8'bx1000000}: int_pri_ir = 1;
        {8'b10000000}: int_pri_ir = 0;
        default 
        int_pri_ir<=3'bxxx;
        /*
        each interrupt request bit is 0 ,namely,there are not bits setted 
        the int_o =0 && (pri_in < int_pri_ir) =0,(see below)
        so we can safly set  int_pri_ir = 3'bxxx as default to reduce the logic resource usage.
        */
    endcase

/*
if there is a interruput and the priority is greater than the current 
interrupt priority,the generate a gloable interrput request signal.
*/
    always@(*)
        int_o  =  (valid_ir_data!=0)&&(pri_in < int_pri_ir);
		
/*
there bits interrupt number
*/
    always@(*)
        irq_no = int_pri_ir[2:0] ;

endmodule
