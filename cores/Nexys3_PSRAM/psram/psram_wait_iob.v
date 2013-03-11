`timescale 1ns/1ps

module psram_wait_iob
(
    input              iff_d,
    output             iff_q,
    input              iff_clk,
    input              iff_en
);

//attribute iob                 : string;
//attribute iob of psram_wait_iob_iff : label is "true";

(* iob = "true" *) FDRSE #(
    .INIT(1'b0)
) psram_wait_iob_iff (
    .Q(iff_q),
    .C(iff_clk),
    .CE(1'b1),
    .D(iff_d),
    .R(1'b0),
    .S(iff_en)
);

endmodule
