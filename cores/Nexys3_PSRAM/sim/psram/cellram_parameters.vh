/****************************************************************************************
*
*   Disclaimer   This software code and all associated documentation, comments or other
*  of Warranty:  information (collectively "Software") is provided "AS IS" without
*                warranty of any kind. MICRON TECHNOLOGY, INC. ("MTI") EXPRESSLY
*                DISCLAIMS ALL WARRANTIES EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED
*                TO, NONINFRINGEMENT OF THIRD PARTY RIGHTS, AND ANY IMPLIED WARRANTIES
*                OF MERCHANTABILITY OR FITNESS FOR ANY PARTICULAR PURPOSE. MTI DOES NOT
*                WARRANT THAT THE SOFTWARE WILL MEET YOUR REQUIREMENTS, OR THAT THE
*                OPERATION OF THE SOFTWARE WILL BE UNINTERRUPTED OR ERROR-FREE.
*                FURTHERMORE, MTI DOES NOT MAKE ANY REPRESENTATIONS REGARDING THE USE OR
*                THE RESULTS OF THE USE OF THE SOFTWARE IN TERMS OF ITS CORRECTNESS,
*                ACCURACY, RELIABILITY, OR OTHERWISE. THE ENTIRE RISK ARISING OUT OF USE
*                OR PERFORMANCE OF THE SOFTWARE REMAINS WITH YOU. IN NO EVENT SHALL MTI,
*                ITS AFFILIATED COMPANIES OR THEIR SUPPLIERS BE LIABLE FOR ANY DIRECT,
*                INDIRECT, CONSEQUENTIAL, INCIDENTAL, OR SPECIAL DAMAGES (INCLUDING,
*                WITHOUT LIMITATION, DAMAGES FOR LOSS OF PROFITS, BUSINESS INTERRUPTION,
*                OR LOSS OF INFORMATION) ARISING OUT OF YOUR USE OF OR INABILITY TO USE
*                THE SOFTWARE, EVEN IF MTI HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH
*                DAMAGES. Because some jurisdictions prohibit the exclusion or
*                limitation of liability for consequential or incidental damages, the
*                above limitation may not apply to you.
*
*                Copyright 2003 Micron Technology, Inc. All rights reserved.
*
****************************************************************************************/

    // Parameters current with P26Z datasheet rev H

    // Timing parameters based on Speed Grade

                             // SYMBOL UNITS DESCRIPTION
                             // ------ ----- -----------
`ifdef sg7013
    parameter tACLK =   7.0; // ns CLK to output delay
    parameter tAPA  =  20.0; // ns Page access time
    parameter tAW   =  70.0; // ns Address valid to end of WRITE
    parameter tBW   =  70.0; // ns BY# select to end of WRITE
    parameter tCBPH =   5.0; // ns CE# HIGH between subsequent burst or mixed-mode operations
    parameter tCLK  =   7.5; // ns CLK period
    parameter tCSP  =   2.5; // ns CE# setup time to active CLK edge
    parameter tCW   =  70.0; // ns Chip enable to end of WRITE
    parameter tHD   =   1.5; // ns Hold time from active CLK edge
    parameter tKP   =   3.0; // ns CLK HIGH or LOW time
    parameter tPC   =  20.0; // ns Page READ cycle time
    parameter tRC   =  70.0; // ns READ cycle time
    parameter tSP   =   2.0; // ns Setup time to active CLK edge
    parameter tVP   =   5.0; // ns ADV# pulse width LOW
    parameter tVS   =  70.0; // ns ADV# setup to end of WRITE
    parameter tWC   =  70.0; // ns WRITE cycle time
    parameter tWP   =  45.0; // ns WRITE pulse width
`else `ifdef sg701
    parameter tACLK =   7.0; // ns CLK to output delay
    parameter tAPA  =  20.0; // ns Page access time
    parameter tAW   =  70.0; // ns Address valid to end of WRITE
    parameter tBW   =  70.0; // ns BY# select to end of WRITE
    parameter tCBPH =   5.0; // ns CE# HIGH between subsequent burst or mixed-mode operations
    parameter tCLK  =  9.62; // ns CLK period
    parameter tCSP  =   3.0; // ns CE# setup time to active CLK edge
    parameter tCW   =  70.0; // ns Chip enable to end of WRITE
    parameter tHD   =   2.0; // ns Hold time from active CLK edge
    parameter tKP   =   3.0; // ns CLK HIGH or LOW time
    parameter tPC   =  20.0; // ns Page READ cycle time
    parameter tRC   =  70.0; // ns READ cycle time
    parameter tSP   =   3.0; // ns Setup time to active CLK edge
    parameter tVP   =   5.0; // ns ADV# pulse width LOW
    parameter tVS   =  70.0; // ns ADV# setup to end of WRITE
    parameter tWC   =  70.0; // ns WRITE cycle time
    parameter tWP   =  45.0; // ns WRITE pulse width
`else `ifdef sg708
    parameter tACLK =   9.0; // ns CLK to output delay
    parameter tAPA  =  20.0; // ns Page access time
    parameter tAW   =  70.0; // ns Address valid to end of WRITE
    parameter tBW   =  70.0; // ns BY# select to end of WRITE
    parameter tCBPH =   6.0; // ns CE# HIGH between subsequent burst or mixed-mode operations
    parameter tCLK  =  12.5; // ns CLK period
    parameter tCSP  =   4.0; // ns CE# setup time to active CLK edge
    parameter tCW   =  70.0; // ns Chip enable to end of WRITE
    parameter tHD   =   2.0; // ns Hold time from active CLK edge
    parameter tKP   =   4.0; // ns CLK HIGH or LOW time
    parameter tPC   =  20.0; // ns Page READ cycle time
    parameter tRC   =  70.0; // ns READ cycle time
    parameter tSP   =   3.0; // ns Setup time to active CLK edge
    parameter tVP   =   5.0; // ns ADV# pulse width LOW
    parameter tVS   =  70.0; // ns ADV# setup to end of WRITE
    parameter tWC   =  70.0; // ns WRITE cycle time
    parameter tWP   =  45.0; // ns WRITE pulse width
`else `define sg856
    parameter tACLK =  11.0; // ns CLK to output delay
    parameter tAPA  =  25.0; // ns Page access time
    parameter tAW   =  85.0; // ns Address valid to end of WRITE
    parameter tBW   =  85.0; // ns BY# select to end of WRITE
    parameter tCBPH =   8.0; // ns CE# HIGH between subsequent burst or mixed-mode operations
    parameter tCLK  =  15.0; // ns CLK period
    parameter tCSP  =   5.0; // ns CE# setup time to active CLK edge
    parameter tCW   =  85.0; // ns Chip enable to end of WRITE
    parameter tHD   =   2.0; // ns Hold time from active CLK edge
    parameter tKP   =   5.0; // ns CLK HIGH or LOW time
    parameter tPC   =  25.0; // ns Page READ cycle time
    parameter tRC   =  85.0; // ns READ cycle time
    parameter tSP   =   3.0; // ns Setup time to active CLK edge
    parameter tVP   =   7.0; // ns ADV# pulse width LOW
    parameter tVS   =  85.0; // ns ADV# setup to end of WRITE
    parameter tWC   =  85.0; // ns WRITE cycle time
    parameter tWP   =  55.0; // ns WRITE pulse width
`endif `endif `endif
    parameter tAS   =   0.0; // ns Address and ADV# LOW setup time
    parameter tAVH  =   2.0; // ns Address hold from ADV# HIGH
    parameter tAVS  =   5.0; // ns Address setup to ADV# HIGH
    parameter tCPH  =   5.0; // ns CE# HIGH between subsequent async operations
    parameter tCVS  =   7.0; // ns CE# LOW to ADV# HIGH
    parameter tDH   =   0.0; // ns Data hold from WRITE time
    parameter tDPDX =  10e3; // ns CE# LOW time to exit DPD
    parameter tDW   =  20.0; // ns Data WRITE setup time
    parameter tOW   =   5.0; // ns End WRITE to Low-Z output
    parameter tPU   = 150e3; // ns Initialization period (required before normal operations)
    parameter tVPH  =   0.0; // ns ADV# pulse width HIGH
    parameter tWPH  =  10.0; // ns WRITE pulse width HIGH
    parameter tWR   =   0.0; // ns WRITE recovery time


//Dafuq? Why doesn't this specparam stuff work? Copied that shit down below and renamed it to normal parameters...
/*specify
`ifdef sg7013
    specparam tAA   =  70.0; // ns Address access time
    specparam tAADV =  70.0; // ns ADV# access time
    specparam tABA  =  35.5; // ns Burst to read access time (variable latency)
    specparam tBA   =  70.0; // ns BY# access time
    specparam tCO   =  70.0; // ns Chip select access time
    specparam tHZ   =   7.0; // ns Chip disable to High-Z output
    specparam tKHTL =   7.0; // ns CLK to WAIT valid
    specparam tOHZ  =   7.0; // ns Output disable to High-Z output
`else `ifdef sg701
    specparam tAA   =  70.0; // ns Address access time
    specparam tAADV =  70.0; // ns ADV# access time
    specparam tABA  =  35.9; // ns Burst to read access time (variable latency)
    specparam tBA   =  70.0; // ns BY# access time
    specparam tCO   =  70.0; // ns Chip select access time
    specparam tHZ   =   8.0; // ns Chip disable to High-Z output
    specparam tKHTL =   7.0; // ns CLK to WAIT valid
    specparam tOHZ  =   8.0; // ns Output disable to High-Z output
`else `ifdef sg708
    specparam tAA   =  70.0; // ns Address access time
    specparam tAADV =  70.0; // ns ADV# access time
    specparam tABA  =  46.5; // ns Burst to read access time (variable latency)
    specparam tBA   =  70.0; // ns BY# access time
    specparam tCO   =  70.0; // ns Chip select access time
    specparam tHZ   =   8.0; // ns Chip disable to High-Z output
    specparam tKHTL =   9.0; // ns CLK to WAIT valid
    specparam tOHZ  =   8.0; // ns Output disable to High-Z output
`else `ifdef sg856
    specparam tAA   =  85.0; // ns Address access time
    specparam tAADV =  85.0; // ns ADV# access time
    specparam tABA  =  55.0; // ns Burst to read access time (variable latency)
    specparam tBA   =  85.0; // ns BY# access time
    specparam tCO   =  85.0; // ns Chip select access time
    specparam tHZ   =   8.0; // ns Chip disable to High-Z output
    specparam tKHTL =  11.0; // ns CLK to WAIT valid
    specparam tOHZ  =   8.0; // ns Output disable to High-Z output
`endif `endif `endif `endif
    specparam tAHZ  =   3.0; // ns ADV# HIGH to AD/Q Low-Z output
    specparam tALZ  =   8.0; // ns ADV# LOW to AD/Q High-Z output
    specparam tBHZ  =   8.0; // ns BY# disable to High-Z output
    specparam tBLZ  =  10.0; // ns LB#/UB# enable to Low-Z output
    specparam tBOE  =  20.0; // ns Burst OE# LOW to output delay CE# HIGH between subsequent burst or mixedmode operations
    specparam tCEM  =   4e3; // ns Maximum CE# pulse width
    specparam tCEW_MIN= 1.0; // ns Minimum CE# LOW to WAIT valid
    specparam tCEW_MAX= 7.5; // ns Maximum CE# LOW to WAIT valid
    specparam tDPD  =  10e3; // ns Time from DPD entry to DPD exit
    specparam tKOH  =   2.0; // ns Output HOLD from CLK
    specparam tLZ   =  10.0; // ns Chip enable to Low-Z output
    specparam tOE   =  20.0; // ns Output enable to valid output
    specparam tOEW_MIN= 1.0; // ns OE# LOW to WAIT valid
    specparam tOEW_MAX= 7.5; // ns OE# LOW to WAIT valid
    specparam tOH   =   5.0; // ns Output hold from address change
    specparam tOLZ  =   3.0; // ns Output enable to Low-Z output
    specparam tWHZ  =   8.0; // ns WRITE to A/DQ High-Z output
    specparam tWI   =  20.0; // ns time WRITE invalid
endspecify*/

`ifdef sg7013
    parameter tAA   =  70.0; // ns Address access time
    parameter tAADV =  70.0; // ns ADV# access time
    parameter tABA  =  35.5; // ns Burst to read access time (variable latency)
    parameter tBA   =  70.0; // ns BY# access time
    parameter tCO   =  70.0; // ns Chip select access time
    parameter tHZ   =   7.0; // ns Chip disable to High-Z output
    parameter tKHTL =   7.0; // ns CLK to WAIT valid
    parameter tOHZ  =   7.0; // ns Output disable to High-Z output
`else `ifdef sg701
    parameter tAA   =  70.0; // ns Address access time
    parameter tAADV =  70.0; // ns ADV# access time
    parameter tABA  =  35.9; // ns Burst to read access time (variable latency)
    parameter tBA   =  70.0; // ns BY# access time
    parameter tCO   =  70.0; // ns Chip select access time
    parameter tHZ   =   8.0; // ns Chip disable to High-Z output
    parameter tKHTL =   7.0; // ns CLK to WAIT valid
    parameter tOHZ  =   8.0; // ns Output disable to High-Z output
`else `ifdef sg708
    parameter tAA   =  70.0; // ns Address access time
    parameter tAADV =  70.0; // ns ADV# access time
    parameter tABA  =  46.5; // ns Burst to read access time (variable latency)
    parameter tBA   =  70.0; // ns BY# access time
    parameter tCO   =  70.0; // ns Chip select access time
    parameter tHZ   =   8.0; // ns Chip disable to High-Z output
    parameter tKHTL =   9.0; // ns CLK to WAIT valid
    parameter tOHZ  =   8.0; // ns Output disable to High-Z output
`else `ifdef sg856
    parameter tAA   =  85.0; // ns Address access time
    parameter tAADV =  85.0; // ns ADV# access time
    parameter tABA  =  55.0; // ns Burst to read access time (variable latency)
    parameter tBA   =  85.0; // ns BY# access time
    parameter tCO   =  85.0; // ns Chip select access time
    parameter tHZ   =   8.0; // ns Chip disable to High-Z output
    parameter tKHTL =  11.0; // ns CLK to WAIT valid
    parameter tOHZ  =   8.0; // ns Output disable to High-Z output
`endif `endif `endif `endif
    parameter tAHZ  =   3.0; // ns ADV# HIGH to AD/Q Low-Z output
    parameter tALZ  =   8.0; // ns ADV# LOW to AD/Q High-Z output
    parameter tBHZ  =   8.0; // ns BY# disable to High-Z output
    parameter tBLZ  =  10.0; // ns LB#/UB# enable to Low-Z output
    parameter tBOE  =  20.0; // ns Burst OE# LOW to output delay CE# HIGH between subsequent burst or mixedmode operations
    parameter tCEM  =   4e3; // ns Maximum CE# pulse width
    parameter tCEW_MIN= 1.0; // ns Minimum CE# LOW to WAIT valid
    parameter tCEW_MAX= 7.5; // ns Maximum CE# LOW to WAIT valid
    parameter tDPD  =  10e3; // ns Time from DPD entry to DPD exit
    parameter tKOH  =   2.0; // ns Output HOLD from CLK
    parameter tLZ   =  10.0; // ns Chip enable to Low-Z output
    parameter tOE   =  20.0; // ns Output enable to valid output
    parameter tOEW_MIN= 1.0; // ns OE# LOW to WAIT valid
    parameter tOEW_MAX= 7.5; // ns OE# LOW to WAIT valid
    parameter tOH   =   5.0; // ns Output hold from address change
    parameter tOLZ  =   3.0; // ns Output enable to Low-Z output
    parameter tWHZ  =   8.0; // ns WRITE to A/DQ High-Z output
    parameter tWI   =  20.0; // ns time WRITE invalid


// Size Parameters based on Part Width
`define x16
    parameter ADQ_BITS       = 23;
    parameter DQ_BITS        = 16;
    parameter BY_BITS        = 2;

    parameter ADDR_BITS      = 23;
    parameter COL_BITS       = 7;           // DIDR[15] = 128 words per row
    parameter MEM_BITS       = 10;

    parameter BCR            = 2'b10;
    parameter RCR            = 2'b00;
    parameter DIDR           = 2'b01;
    parameter REG_SEL        = 18;

    parameter CR10           = 2'b01;
    parameter CR15           = 2'b10;
    parameter CR20           = 2'b11;
    parameter GENERATION     = 2'b10;       // DIDR[7:5] = CR1.5

    parameter CR20WAIT_POLARITY = 1'b1;     // 0 = Active Low, 1 = Active High
    parameter CRE_READ       = 1'b1;        // allow READ using CRE to BCR/RCR
    parameter BCR_MASK       = 18'b11_1_1_111_1_0_1_00_11_1_111; // valid bits in BCR
    parameter BCR_DEFAULT    =    16'b1_0_011_1_0_1_00_01_1_111;
    parameter RCR_MASK       = 18'b11_00000000_1_00_1_0_111; // valid bits in RCR
    parameter RCR_DEFAULT    =    16'b00000000_0_00_1_0_000;
    parameter DIDR_MASK      = 18'b11_1_1111_111_111_11111; // valid bits in DIDR
    parameter DIDR_DEFAULT   =    16'b0_0000_011_010_00011;

// Function to return the minimum clock period
function real min_clk_period;
    input initial_latency;
    input [2:0] latency_counter;
    begin
        min_clk_period = 0.0;
        if (initial_latency) begin // fixed
`ifdef sg7013
            case (latency_counter)
                3'd2   : min_clk_period = 30.00;
                3'd3   : min_clk_period = 19.20;
                3'd4   : min_clk_period = 15.00;
                3'd5   : min_clk_period = 13.30;
                3'd6   : min_clk_period =  9.62;
                3'd0   : min_clk_period =  7.50;
            endcase
`else `ifdef sg701
            case (latency_counter)
                3'd2   : min_clk_period = 30.00;
                3'd3   : min_clk_period = 19.20;
                3'd4   : min_clk_period = 15.00;
                3'd5   : min_clk_period = 13.30;
                3'd6   : min_clk_period =  9.62;
                3'd0   : min_clk_period =  9.62;
            endcase
`else `ifdef sg708
            case (latency_counter)
                3'd2   : min_clk_period = 30.00;
                3'd3   : min_clk_period = 19.20;
                3'd4   : min_clk_period = 15.00;
                3'd5   : min_clk_period = 13.30;
                3'd6   : min_clk_period = 12.50;
                3'd0   : min_clk_period = 12.50;
            endcase
`else `ifdef sg856
            case (latency_counter)
                3'd2   : min_clk_period = 50.00;
                3'd3   : min_clk_period = 30.00;
                3'd4   : min_clk_period = 25.00;
                3'd5   : min_clk_period = 19.20;
                3'd6   : min_clk_period = 15.00;
                3'd0   : min_clk_period = 15.00;
            endcase
`endif `endif `endif `endif
        end else begin // variable
`ifdef sg7013
            case (latency_counter)
                3'd2   : min_clk_period = 15.00;
                3'd3   : min_clk_period =  9.62;
                3'd4   : min_clk_period =  7.50;
            endcase
`else `ifdef sg701
            case (latency_counter)
                3'd2   : min_clk_period = 15.00;
                3'd3   : min_clk_period =  9.62;
            endcase
`else `ifdef sg708
            case (latency_counter)
                3'd2   : min_clk_period = 19.20;
                3'd3   : min_clk_period = 12.50;
            endcase
`else `ifdef sg856
            case (latency_counter)
                3'd2   : min_clk_period = 25.00;
                3'd3   : min_clk_period = 15.00;
            endcase
`endif `endif `endif `endif
        end
    end
endfunction

