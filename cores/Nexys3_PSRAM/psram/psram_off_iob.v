`timescale 1ns/1ps

module psram_off_iob
(
    input              off_d,
    output             off_q,
    input              off_clk
);

//attribute iob                 : string;
//attribute iob of psram_off_iob : label is "true";

(* iob = "true" *) FDRSE #(
    .INIT(1'b1)
) psram_off_iob (
    .Q(off_q),
    .C(off_clk),
    .CE(1'b1),
    .D(off_d),
    .R(1'b0),
    .S(1'b0)
);

endmodule
