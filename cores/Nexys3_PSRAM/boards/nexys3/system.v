`timescale 1ns/1ps

module system (
    input clk,
    input btns,
    output [7:0] leds,

    output [22:0] mem_addr,
    output mem_clk,
    output mem_ce_n,
    output mem_oe_n,
    output mem_we_n,
    output mem_adv_n,
    output [1:0] mem_be,
    output mem_cre,
    input  mem_wait,
    inout [15:0] mem_data
);

wire rst_n;
assign rst_n = ~btns;

reg [22:0]  fml_adr;
reg         fml_we;
reg         fml_stb;
wire        fml_eack;
reg [7:0]   fml_sel;
reg [63:0]  fml_di;
wire [63:0] fml_do;

wire        ctrl_idle;

wire [15:0] mem_data_i;
wire [15:0] mem_data_o;
wire [15:0] mem_data_t;

wire controller_ready;

psram_sync psram_sync_inst(
    .clk(clk),
    .rst_n(rst_n),

    .controller_ready(controller_ready),

    .fml_adr(fml_adr),
    .fml_stb(fml_stb),
    .fml_we(fml_we),
    .fml_eack(fml_eack),
    .fml_sel(fml_sel),
    .fml_di(fml_di),
    .fml_do(fml_do),

    .mem_addr(mem_addr),
    .mem_clk(mem_clk),
    .mem_cen(mem_ce_n),
    .mem_cre(mem_cre),
    .mem_oen(mem_oe_n),
    .mem_wen(mem_we_n),
    .mem_adv(mem_adv_n),
    .mem_be(mem_be),
    .mem_wait(mem_wait),
    .mem_data_i(mem_data_i),
    .mem_data_o(mem_data_o),
    .mem_data_t(mem_data_t)
);

//Tri-State memory inout stuff
assign mem_data = mem_data_t[0] ? mem_data_o : 16'bZ;
assign mem_data_i = mem_data;

parameter s_waitready = 4'd0;
parameter s_write = 4'd1;
parameter s_waitwrite = 4'd2;
parameter s_read = 4'd3;
parameter s_waitread = 4'd4;
parameter s_done = 4'd5;

reg [3:0] cur_state;
reg [3:0] next_state;

reg [7:0] ledoutput;
assign leds = ledoutput;

reg [7:0] cntr;

always @(posedge clk or negedge rst_n) begin
    if(~rst_n) begin
        cur_state <= s_waitready;
        next_state = s_waitready;
        ledoutput = 0;

    end else begin
        fml_adr = 23'b0;
        fml_we = 1'b0;
        fml_stb = 1'b0;
        fml_sel = 8'hFF;
        fml_di = 64'hAA55133712345678;

        case(cur_state)
            s_waitready:
                if(controller_ready)
                    next_state = s_write;
                else
                    next_state = s_waitready;

            s_write: begin
                fml_stb = 1'b1;
                fml_we = 1'b1;
                next_state = s_waitwrite;
            end

            s_waitwrite: begin
                if(fml_eack) begin
                    $display("got write ack \\o/");
                    next_state = s_read;
                end else
                    next_state = s_waitwrite;
            end

            s_read: begin
                fml_stb = 1'b1;
                next_state = s_waitread;
            end

            s_waitread: begin
                if(fml_eack) begin
                    ledoutput = fml_do[63:56];
                    $display("got read ack \\o/ %x%x", fml_do[63:32], fml_do[31:0],);
                    next_state = s_done;
                end else
                    next_state = s_waitread;
            end

            s_done:
                next_state = s_done;

            default:
                next_state = s_waitready;
        endcase

        //advance to next state
        cur_state <= next_state;
    end
end

endmodule
