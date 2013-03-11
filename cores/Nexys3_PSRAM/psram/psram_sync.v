`timescale 1ns/1ps

module psram_sync
(
    //System signals
    input              clk,         //100MHz system clock
    input              rst_n,       //reset

    output             controller_ready,

    //FML Interface
    input      [22:0]  fml_adr,
    input              fml_stb,
    input              fml_we,
    output             fml_eack,
    input      [7:0]   fml_sel,
    input      [63:0]  fml_di,
    output     [63:0]  fml_do,

    //Memory interface
    output     [22:0]  mem_addr,   //Memory address
    output             mem_clk,    //Memory clock
    output             mem_cen,    //Memory chip enable
    output             mem_cre,    //Memory control register enable
    output             mem_oen,    //Memory output enable
    output             mem_wen,    //Memory write enable
    output             mem_adv,    //Memory address valid
    input              mem_wait,   //Memory wait
    output     [ 1:0]  mem_be,     //Memory byte enable

    input      [15:0]  mem_data_i, //Memory data in
    output     [15:0]  mem_data_o, //Memory data out
    output     [15:0]  mem_data_t  //Memory data tri-state
);

//internal signals
wire        mem_clk_en;
wire [15:0] mem_data_i_int;
wire [15:0] mem_data_o_int;
wire        mem_data_oe_int;
wire [22:0] mem_addr_int;
wire [ 1:0] mem_be_int;
wire        mem_wen_int;
wire        mem_oen_int;
wire        mem_cen_int;
wire        mem_adv_int;
//wire        mem_wait_int;
wire        mem_cre_int;

//off-clock (shifted by 180 degress)
wire        off_clk = ~clk;

//clock IOB
psram_clk_iob psram_clk_iob_1(
    .clk(clk),
    .clk_en(mem_clk_en),
    .clk_q(mem_clk)
);

//data IOBs
genvar i;
generate
    for(i=0; i<16; i=i+1) begin
        //data in- and output
        psram_data_iob psram_data_iob_1(
            .iff_d(mem_data_i[i]),
            .iff_q(mem_data_i_int[i]),
            .iff_clk(clk),

            .off_d(mem_data_o_int[i]),
            .off_q(mem_data_o[i]),
            .off_clk(off_clk)
        );

        //if oe is on, switch tri-state to output
        psram_off_iob psram_data_oe_off_1(
            .off_d(mem_data_oe_int),
            .off_q(mem_data_t[i]),
            .off_clk(off_clk)
        );
    end
endgenerate

//address iobs
generate
    for(i=0; i<23; i=i+1) begin
        psram_off_iob psram_addr_off_1(
            .off_d(mem_addr_int[i]),
            .off_q(mem_addr[i]),
            .off_clk(off_clk)
        );
    end
endgenerate

//byte enable iobs
generate
    for(i=0; i<2; i=i+1) begin
        psram_off_iob psram_be_off_1(
            .off_d(mem_be_int[i]),
            .off_q(mem_be[i]),
            .off_clk(off_clk)
        );
    end
endgenerate

//control signals
psram_off_iob psram_wen_off_1(
    .off_d(mem_wen_int),
    .off_q(mem_wen),
    .off_clk(off_clk)
);

psram_off_iob psram_oen_off_1(
    .off_d(mem_oen_int),
    .off_q(mem_oen),
    .off_clk(off_clk)
);

psram_off_iob psram_cen_off_1(
    .off_d(mem_cen_int),
    .off_q(mem_cen),
    .off_clk(off_clk)
);

psram_off_iob psram_adv_off_1(
    .off_d(mem_adv_int),
    .off_q(mem_adv),
    .off_clk(off_clk)
);

psram_off_iob psram_cre_off_1(
    .off_d(mem_cre_int),
    .off_q(mem_cre),
    .off_clk(off_clk)
);

//controller statemachine instance
psram_ctrlr psram_ctrlr_1(
    .clk(clk),
    .rst_n(rst_n),

    .controller_ready(controller_ready),

    //FML interface
    .fml_adr(fml_adr),
    .fml_stb(fml_stb),
    .fml_we(fml_we),
    .fml_eack(fml_eack),
    .fml_sel(fml_sel),
    .fml_di(fml_di),
    .fml_do(fml_do),

    //Memory interface
    .mem_clk_en(mem_clk_en),
    .mem_data_i_int(mem_data_i_int),
    .mem_data_o_int(mem_data_o_int),
    .mem_data_oe_int(mem_data_oe_int),
    .mem_addr_int(mem_addr_int),
    .mem_be_int(mem_be_int),
    .mem_wen_int(mem_wen_int),
    .mem_oen_int(mem_oen_int),
    .mem_cen_int(mem_cen_int),
    .mem_adv_int(mem_adv_int),
    .mem_cre_int(mem_cre_int),
    .mem_wait_int(mem_wait)
);

endmodule
