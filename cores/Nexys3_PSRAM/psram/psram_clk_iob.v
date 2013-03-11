`timescale 1ns/1ps

module psram_clk_iob
(
    input              clk,
    input              clk_en,
    output             clk_q
);

//attribute iob                 : string;
//attribute iob of psram_clk_iob : label is "true";

wire c0, c1, clk_en_n;
assign c0 = clk;
assign c1 = ~clk;
assign clk_en_n = ~clk_en;

(* iob = "true" *) FDDRRSE psram_clk_iob(
    .Q(clk_q),
    .C0(c0),
    .C1(c1),
    .CE(1'b1),
    .D0(1'b1),
    .D1(1'b0),
    .R(clk_en_n),
    .S(1'b0)
);

endmodule
