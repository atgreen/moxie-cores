`timescale 1ns/1ps

module psram_ctrlr
(
    //System signals
    input              clk,         //100MHz system clock
    input              rst_n,       //reset
    output reg         controller_ready,

    //FML Interface
    input      [22:0]  fml_adr,
    input              fml_stb,
    input              fml_we,
    output reg         fml_eack,
    input      [7:0]   fml_sel,
    input      [63:0]  fml_di,
    output reg [63:0]  fml_do,

    //Memory interface
    output reg         mem_clk_en,
    input      [15:0]  mem_data_i_int,
    output reg [15:0]  mem_data_o_int,
    output reg         mem_data_oe_int,
    output     [22:0]  mem_addr_int,
    output reg [ 1:0]  mem_be_int,
    output reg         mem_wen_int,
    output reg         mem_oen_int,
    output reg         mem_cen_int,
    output reg         mem_adv_int,
    output reg         mem_cre_int,
    input              mem_wait_int
);

reg [11:0] state;
reg [11:0] next_state;

parameter s_startup = 0;
parameter s_write_bcr1 = 1;
parameter s_write_bcr2 = 2;
parameter s_write_bcr3 = 3;
parameter s_idle = 4;
parameter s_write1 = 5;
parameter s_write2 = 6;
parameter s_write3 = 7;
parameter s_write4 = 8;
parameter s_write5 = 9;
parameter s_write6 = 10;
parameter s_write7 = 11;
parameter s_write8 = 12;
parameter s_write9 = 13;
parameter s_write10 = 14;
parameter s_write11 = 15;
parameter s_read1 = 16;
parameter s_read2 = 17;
parameter s_read3 = 18;
parameter s_read4 = 19;
parameter s_read5 = 29;
parameter s_read6 = 21;
parameter s_read7 = 22;
parameter s_read8 = 23;
parameter s_read9 = 24;
parameter s_read10 = 25;
parameter s_read11 = 26;
parameter s_read12 = 27;

always @(posedge clk or negedge rst_n) begin
    if(~rst_n) begin
        state <= s_startup;
    end else begin
        state <= next_state;
    end
end

reg [14:0] cntr;
reg [14:0] cntr_reload_val;
always @(posedge clk or negedge rst_n) begin
    if(~rst_n) begin
        cntr <= 15001;
    end else if(cntr != 0)
        cntr <= cntr - 1'b1;
    else
        cntr <= cntr_reload_val;
end

//Address, data and byte select registers
reg [22:0] addr;
reg [63:0] data;
reg [7:0]  be;
reg        latch_addr;
reg        latch_data;
reg        latch_be;
always @(posedge clk or negedge rst_n) begin
    if(~rst_n) begin
        addr <= 23'b000_10_00_0_1_110_1_0_1_0_0_01_1_111;
        data <= 63'b0;
        be <= 8'b0;
    end else begin
        if(latch_addr)
            addr <= fml_adr;
        if(latch_data)
            data <= fml_di;
        if(latch_be)
            be <= ~fml_sel;
    end
end
assign mem_addr_int = addr;


always @(*) begin
    controller_ready = 1'b1;

    next_state = state;

    mem_clk_en = 1'b0;
    fml_eack = 1'b0;

    mem_data_o_int = 16'b0;
    mem_data_oe_int = 1'b0;

    mem_be_int = 2'b0;
    mem_wen_int = 1'b1;
    mem_oen_int = 1'b1;
    mem_cen_int = 1'b1;
    mem_adv_int = 1'b1;
    mem_cre_int = 1'b0;

    cntr_reload_val = 15'b0;

    latch_addr = 1'b0;
    latch_data = 1'b0;
    latch_be = 1'b0;

    case(state)
        s_startup: begin
            controller_ready = 1'b0;
            if(cntr == 0)
                next_state = s_write_bcr1;
        end

        s_write_bcr1: begin
            controller_ready = 1'b0;
            mem_cre_int = 1'b1;
            mem_cen_int = 1'b0;
            mem_adv_int = 1'b0;
            next_state = s_write_bcr2;
        end

        s_write_bcr2: begin
            controller_ready = 1'b0;
            mem_cre_int = 1'b1;
            mem_cen_int = 1'b0;
            cntr_reload_val = 15'd5;
            next_state = s_write_bcr3;
        end

        s_write_bcr3: begin
            controller_ready = 1'b0;
            mem_cen_int = 1'b0;
            mem_wen_int = 1'b0;
            if(cntr == 0)
                next_state = s_idle;
        end

        s_idle: begin
            if(fml_stb && fml_we) begin
                //write request
                //latch data, address and byte select
                latch_addr = 1'b1;
                latch_data = 1'b1;
                latch_be = 1'b1;
                next_state = s_write1;
            end else if(fml_stb && !fml_we) begin
                //read request
                latch_addr = 1'b1;
                next_state = s_read1;
            end
        end

        //-------------- read --------------

        s_read1: begin
            mem_clk_en = 1'b1;
            mem_cen_int = 1'b0;
            mem_adv_int = 1'b0;
            next_state = s_read2;
        end

        s_read2: begin
            mem_clk_en = 1'b1;
            mem_cen_int = 1'b0;

            //Only works in simulation :(
            /*if(mem_wait_int == 0)
                next_state = s_read8;*/

            next_state = s_read3;
        end

        s_read3: begin
            mem_clk_en = 1'b1;
            mem_cen_int = 1'b0;
            next_state = s_read4;
        end

        s_read4: begin
            mem_clk_en = 1'b1;
            mem_cen_int = 1'b0;
            next_state = s_read5;
        end

        s_read5: begin
            mem_clk_en = 1'b1;
            mem_cen_int = 1'b0;
            next_state = s_read6;
        end

        s_read6: begin
            mem_clk_en = 1'b1;
            mem_cen_int = 1'b0;
            next_state = s_read7;
        end

        s_read7: begin
            mem_clk_en = 1'b1;
            mem_cen_int = 1'b0;
            mem_oen_int = 1'b0;
            next_state = s_read8;
        end

        s_read8: begin
            mem_clk_en = 1'b1;
            mem_cen_int = 1'b0;
            mem_oen_int = 1'b0;
            next_state = s_read9;
        end

        s_read9: begin
            mem_clk_en = 1'b1;
            mem_cen_int = 1'b0;
            mem_oen_int = 1'b0;
            fml_do[15:0] = mem_data_i_int;
            next_state = s_read10;
        end

        s_read10: begin
            mem_clk_en = 1'b1;
            mem_cen_int = 1'b0;
            mem_oen_int = 1'b0;
            fml_do[31:16] = mem_data_i_int;
            next_state = s_read11;
        end

        s_read11: begin
            mem_cen_int = 1'b0;
            mem_oen_int = 1'b0;
            fml_do[47:32] = mem_data_i_int;
            next_state = s_read12;
        end

        s_read12: begin
            fml_do[63:48] = mem_data_i_int;
            fml_eack = 1'b1;    //ack fml transfer
            next_state = s_idle;
        end

        //-------------- write --------------

        s_write1: begin
            mem_clk_en = 1'b1;
            mem_cen_int = 1'b0;
            mem_adv_int = 1'b0;
            mem_wen_int = 1'b0;
            next_state = s_write2;
        end

        s_write2: begin
            mem_clk_en = 1'b1;
            mem_cen_int = 1'b0;

            //Only works in simulation :(
            /*if(mem_wait_int == 0)
                next_state = s_write7;*/

            next_state = s_write3;
        end

        s_write3: begin
            mem_clk_en = 1'b1;
            mem_cen_int = 1'b0;
            next_state = s_write4;
        end

        s_write4: begin
            mem_clk_en = 1'b1;
            mem_cen_int = 1'b0;
            next_state = s_write5;
        end

        s_write5: begin
            mem_clk_en = 1'b1;
            mem_cen_int = 1'b0;
            next_state = s_write6;
        end

        s_write6: begin
            mem_clk_en = 1'b1;
            mem_cen_int = 1'b0;
            next_state = s_write7;
        end

        s_write7: begin
            mem_clk_en = 1'b1;
            mem_cen_int = 1'b0;
            next_state = s_write8;
        end

        s_write8: begin
            mem_clk_en = 1'b1;
            mem_cen_int = 1'b0;
            mem_data_oe_int = 1'b1;
            mem_data_o_int = data[15:0];
            mem_be_int = be[1:0];
            next_state = s_write9;
        end

        s_write9: begin
            mem_clk_en = 1'b1;
            mem_cen_int = 1'b0;
            mem_data_oe_int = 1'b1;
            mem_data_o_int = data[31:16];
            mem_be_int = be[3:2];
            next_state = s_write10;
        end

        s_write10: begin
            mem_clk_en = 1'b1;
            mem_cen_int = 1'b0;
            mem_data_oe_int = 1'b1;
            mem_data_o_int = data[47:32];
            mem_be_int = be[5:4];
            next_state = s_write11;
        end

        s_write11: begin
            mem_clk_en = 1'b1;
            mem_cen_int = 1'b0;
            mem_data_oe_int = 1'b1;
            mem_data_o_int = data[63:48];
            mem_be_int = be[7:6];
            fml_eack = 1'b1;    //ack fml transfer
            next_state = s_idle;
        end

        default:
            next_state = s_startup;
    endcase
end

endmodule
