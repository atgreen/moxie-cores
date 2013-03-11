//----------------------------------------------------------------------------
//
//----------------------------------------------------------------------------
`timescale 1ns/1ps

module system_tb;

//----------------------------------------------------------------------------
// Parameter (may differ for physical synthesis)
//----------------------------------------------------------------------------
parameter tck              = 10;       // clock period in ns
parameter clk_freq = 1000000000 / tck; // Frequenzy in HZ
//----------------------------------------------------------------------------
//
//----------------------------------------------------------------------------
reg        clk;
reg        reset;
wire [7:0] leds;

wire mem_adv;
wire mem_cre;
wire mem_ce;
wire mem_oe;
wire mem_we;
wire mem_wait;
wire [1:0] mem_be;
wire [22:0] mem_addr;
wire [15:0] mem_data;

//------------------------------------------------------------------
// Decive Under Test
//------------------------------------------------------------------
system #(
) dut  (
	.clk(          clk    ),
	.btns(         reset  ),
    .leds(         leds   ),

	.mem_addr(   mem_addr ),
    .mem_clk(    mem_clk  ),
    .mem_ce_n(   mem_ce   ),
    .mem_oe_n(   mem_oe   ),
    .mem_we_n(   mem_we   ),
    .mem_adv_n(  mem_adv  ),
    .mem_be(     mem_be   ),
    .mem_cre(    mem_cre  ),
    .mem_data(   mem_data ),
    .mem_wait(   mem_wait )
);

//Micron Cellular RAM
cellram cellram_inst (
    .clk(mem_clk),
    .adv_n(mem_adv),
    .cre(mem_cre),
    .o_wait(mem_wait),
    .ce_n(mem_ce),
    .oe_n(mem_oe),
    .we_n(mem_we),
    .lb_n(mem_be[0]),
    .ub_n(mem_be[1]),
    .addr(mem_addr),
    .dq(mem_data)
);

/* Clocking device */
initial         clk <= 1;
always #(tck/2) clk <= ~clk;

/* Simulation setup */
initial begin
	$dumpfile("system_tb.vcd");
	$dumpvars(-1, dut);

	// reset
	#0  reset <= 1;
	#10 reset <= 0;

    #160000 $display("LEDs %x", leds);

	#(tck*20000) $finish;
end

endmodule
