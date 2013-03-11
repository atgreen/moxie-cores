`timescale 1ns/1ps

module psram_data_iob
(
    input              iff_d,
    output             iff_q,
    input              iff_clk,
    input              off_d,
    output             off_q,
    input              off_clk
);

// attribute iob                 : string;
// attribute iob of  psram_data_iob_iff : label is "true";
// attribute iob of  psram_data_iob_off : label is "true";

//input
(* iob = "true" *) FDRSE #(
    .INIT(1'b0)
) psram_data_iob_iff (
    .Q(iff_q),
    .C(iff_clk),
    .CE(1'b1),
    .D(iff_d),
    .R(1'b0),
    .S(1'b0)
);

//output
(* iob = "true" *) FDRSE #(
    .INIT(1'b0)
) psram_data_iob_off (
    .Q(off_q),
    .C(off_clk),
    .CE(1'b1),
    .D(off_d),
    .R(1'b0),
    .S(1'b0)
);

endmodule
