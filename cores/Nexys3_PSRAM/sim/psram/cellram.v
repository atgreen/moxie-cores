/**************************************************************************
*
*    File Name:  cellram.v
*        Model:  BUS Functional
*    Simulator:  Model Technology
*
* Dependencies:  cellram_parameters.vh
*
*       Author:  Micron Technology, Inc.
*        Email:  modelsupport@micron.com
*
*  Description:  Micron 128Mb CellularRAM 2.0 (Async / Burst)
*
*   Disclaimer: This software code and all associated documentation, comments
*               or other information (collectively "Software") is provided
*               "AS IS" without warranty of any kind. MICRON TECHNOLOGY, INC.
*               ("MTI") EXPRESSLY DISCLAIMS ALL WARRANTIES EXPRESS OR IMPLIED,
*               INCLUDING BUT NOT LIMITED TO, NONINFRINGEMENT OF THIRD PARTY
*               RIGHTS, AND ANY IMPLIED WARRANTIES OF MERCHANTABILITY OR FITNESS
*               FOR ANY PARTICULAR PURPOSE. MTI DOES NOT WARRANT THAT THE
*               SOFTWARE WILL MEET YOUR REQUIREMENTS, OR THAT THE OPERATION OF
*               THE SOFTWARE WILL BE UNINTERRUPTED OR ERROR-FREE. FURTHERMORE,
*               MTI DOES NOT MAKE ANY REPRESENTATIONS REGARDING THE USE OR THE
*               RESULTS OF THE USE OF THE SOFTWARE IN TERMS OF ITS CORRECTNESS,
*               ACCURACY, RELIABILITY, OR OTHERWISE. THE ENTIRE RISK ARISING OUT
*               OF USE OR PERFORMANCE OF THE SOFTWARE REMAINS WITH YOU. IN NO
*               EVENT SHALL MTI, ITS AFFILIATED COMPANIES OR THEIR SUPPLIERS BE
*               LIABLE FOR ANY DIRECT, INDIRECT, CONSEQUENTIAL, INCIDENTAL, OR
*               SPECIAL DAMAGES (INCLUDING, WITHOUT LIMITATION, DAMAGES FOR LOSS
*               OF PROFITS, BUSINESS INTERRUPTION, OR LOSS OF INFORMATION)
*               ARISING OUT OF YOUR USE OF OR INABILITY TO USE THE SOFTWARE,
*               EVEN IF MTI HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGES.
*               Because some jurisdictions prohibit the exclusion or limitation
*               of liability for consequential or incidental damages, the above
*               limitation may not apply to you.
*
*               Copyright 2006 Micron Technology, Inc. All rights reserved.
*
* Rev  Author          Date        Changes
* ---  --------------------------  ----------------------------------------
* 1.0  SH              05/10/2003  - Initial release
* 1.1  NB              06/15/2004  - Fixed problem with row crossing
*                                    functionality.
* 1.2  PF              10/20/2004  - Changed default value of
*                                    operating_mode to match datasheet.
*                                    Conditioned sync operation on setting
*                                    operating_mode
* 1.3  dritz           01/20/2005  - Updated parameters, parameterized the model, corrected a few bugs.
*                                      populated BCR, RCR
*                                      added DIDR feature
*                                      fixed burst interrupt on ADV#
*                                      added CR1.0 1.5 features
* 1.4  dritz           10/04/2005  - Fixed sync 2 async transition
* 1.5  dritz           11/31/2005  - o_wait on async corrected.
* 1.6  dritz           02/16/2006  - Row boundary crossing fixed.
* 1.7  dritz           03/21/2006  - Fixed burst read issue (data 1 clk too late with 0 setup)
* 1.8  jmk             06/30/2006  - Added CR2.0 features
* 1.9  jmk             08/02/2006  - Added timing checks.
*                                  - Added delays on output signals.
*                                  - Added associative memory array.
* 2.1  jmk             09/14/2006  - Added cellram_pathdelay module to model
*                                      delays on the read data path.
*                                  - DIDR[19:18] returns 2'b01 during reads.
*                                  - Added tCEM checking on WE#
* 2.2  jmk             09/29/2006  - Added support for x16 part width.
* 3.0  jmk             11/03/2006  - Added ASYNC/PAGE support and ZZ# pin.
*                                  - Added error message for ADV# low for multiple clocks.
*                                  - Entering PAR will corrupt memory.
*                                  - Added tAHZ and tALZ for Mux'd parts.
* 3.2  jmk             01/30/2007  - tHZ, tBHZ, tWHZ, tOHZ minimum = 0
*                                  - tCEW_MIN and tCEW_MAX are checked
*                                  - BY# input port replaced with LB#, UB#
*                                  - Added data corruption when CE# is deasserted prior to the first data transfer.
*                                  - Added checking for illegal burst interruption.
*                                  - Fixed WAIT assertion at end of row during burst write for Cellram 1.5
*                                  - Fixed tAVH checking on ADDR and CRE.
*                                  - Changed the way tOW was checked.
*                                  - Added checking for illegal page mode reads and writes.
* 3.3  jmk             02/09/2007  - Added Cellram 1.0 features.
* 3.4  jmk             04/13/2007  - Removed tCO, tAADV, and tBA requirement during variable latency mode.
* 3.5  jmk             05/30/2007  - Added tVPH checking.
* 3.6  jmk             10/05/2007  - Added support for 8Mb devices (ADDR_BITS=19)
* 3.7  jmk             01/30/2008  - Fixed message for operating mode
* 3.8  jmk             02/28/2008  - Fixed Cellram 1.0 LB#/UB# operation during reads.
**************************************************************************/

// DO NOT CHANGE THE TIMESCALE
// MAKE SURE YOUR SIMULATOR USE "PS" RESOLUTION
`timescale 1ns / 1ps

module cellram (
    clk,
    adv_n,
    cre,
    o_wait,
    ce_n,
    oe_n,
    we_n,
    lb_n,
    ub_n,
    addr,
    dq
);

`include "cellram_parameters.vh"

    // text macros
    `define DQ_PER_BY (DQ_BITS/BY_BITS)

    // Display debug message
    parameter DEBUG         =    'h1;

    // Port declarations
    input                         clk;
    input                         adv_n;
    input                         cre;
    output                        o_wait; // Wait is a keyword in HDL
    input                         ce_n;
    input                         oe_n;
    input                         we_n;
    input                         lb_n;
    input                         ub_n;
    wire            [BY_BITS-1:0] by_n = {ub_n, lb_n};
    input       [ADDR_BITS-1 : 0] addr;
    inout         [DQ_BITS-1 : 0] dq;

    wire                   [31:0] dq_out;
    wire            [DQ_BITS-1:0] dq_in    = dq;
    assign                        dq       = dq_out[DQ_BITS-1:0];
    wire clk_in;
    wire adv_n_in;
    wire cre_in;
    wire zz_n_in;
    wire wait_o;
    reg  par_enabled;
    assign                        clk_in   = clk;
    assign                        adv_n_in = adv_n;
    assign                        cre_in   = (GENERATION == CR20) ? cre | dq_in[DQ_BITS-1] : cre;
    assign                        zz_n_in  = 1'b1;
    assign                        o_wait   = wait_o;
    initial                       par_enabled <= 1'b1; // PAR permanently enabled

    // Memory arrays
    reg             [DQ_BITS-1:0] memory [0:1<<MEM_BITS-1];
    reg           [ADDR_BITS-1:0] memory_addr [0:1<<MEM_BITS-1];
    reg              [MEM_BITS:0] memory_index;
    reg              [MEM_BITS:0] memory_used;

    // Software access registers
    reg                     [1:0] software_access_unlock;
    reg                     [1:0] software_access_which_reg;
    reg                           software_access_rcr_write;

    // System registers
    parameter                     IDLE   = 0;
    parameter                     WR     = 1;
    parameter                     RD     = 2;
    parameter                     CFG_RD = 3;
    parameter                     CFG_WR = 4;

    // Asynchronous registers
    reg                     [2:0] async_state;
    reg                           async_wr_lockout;
    wire                          data_out_enable;
    wire                          data_out_valid;
    wire                          wait_out_enable;
    wire                          wait_out_valid;
    reg                           async_cre;
    reg           [ADDR_BITS-1:0] async_addr;
    reg                           deep_power_down_exit;
    reg                           tow_check;
    reg                           tavh_check;

    // Synchronous registers
    reg                           sync_access;
    reg                           last_access;
    reg                           last_ce;
    reg           [ADDR_BITS-1:0] sync_addr;
    reg                           sync_burst_length_override;
    reg                           sync_cre;
    reg                           sync_we_n;
    reg                     [3:0] sync_latency_cntr;
    reg                           wait_out;
    reg                           sync_wait;
    reg                           sync_wait_q;
    integer                       random_delay;
    integer                       i;

    // Sync to Async transition registers
    reg                   [2:0]   Sync2AsyncConfig;

    // Continuous row end delay
    integer                       row_count;
    reg                           rbc;
    reg             [BY_BITS-1:0] rbc_by;
    reg             [DQ_BITS-1:0] rbc_dq;

    // Refresh Collision
    time                          ref_collision;

    // Output register
    reg             [DQ_BITS-1:0] next_by_out;
    reg             [DQ_BITS-1:0] by_out;
    reg             [DQ_BITS-1:0] next_data_out;
    reg             [DQ_BITS-1:0] data_out;

    // Bus Configuration Register
    reg                     [2:0] burst_length;
    reg                           burst_wrap_n;
    reg                     [1:0] drive_strength;
    reg                           clock_configuration;
    reg                           wait_configuration;
    reg                           cr20wait;
    reg                           cr20wait_code;
    reg                           wait_polarity;
    reg                     [3:0] latency_counter;
    reg                           initial_latency;
    reg                           operating_mode;
    wire                   [15:0] bus_conf_register = {operating_mode, initial_latency, latency_counter[2:0], wait_polarity, cr20wait, wait_configuration, 1'b0, clock_configuration, drive_strength, burst_wrap_n, burst_length};

    // Refresh Configuration Register
    reg                     [2:0] partial_array_refresh;
    reg                           deep_power_down_n;
    reg                     [1:0] case_temp;
    reg                           page_mode;
    reg             [ADDR_BITS:0] partial_address_offset;
    reg             [ADDR_BITS:0] partial_address_size;
    wire                    [7:0] ref_conf_register = {page_mode, case_temp, deep_power_down_n, 1'b0, partial_array_refresh};


    // Device ID Register READ ONLY
    wire                   [15:0] didr_conf_register = DIDR_DEFAULT[15:0];

    // Internal timing
    realtime                      tm_clk_pos;
    realtime                      tm_clk_neg;
    realtime                      tm_clk_period;
    realtime                      tm_ce_n;
    realtime                      tm_tcw;
    realtime                      tm_tdpd;
    realtime                      tm_tdpdx;
    realtime                      tm_adv_n;
    realtime                      tm_adv_n_pos;
    realtime                      tm_adv_n_neg;
    realtime                      tm_we_n;
    realtime                      tm_by_n;
    realtime                      tm_tbw;
    realtime                      tm_cre;
    realtime                      tm_async_cre;
    realtime                      tm_addr;
    realtime                      tm_async_addr;
    realtime                      tm_upper_addr;
    realtime                      tm_dq_tdw; // special case because tDH = 0
    realtime                      tm_page_addr3;
    realtime                      tm_page_addr2;
    realtime                      tm_page_addr1;
    realtime                      tm_page_addr0;
    realtime                      tm_write_end;
    realtime                      tm_power_up;
    realtime                      tm_data_out;
    realtime                      tm_data_delay;


    // Wait buffer
    assign wait_o = wait_out_enable ? (wait_out_valid ? wait_out : 1'bx) : 1'bz;

    // DQ buffer
    wire [31:0] dq_out_buf = (data_out_valid ? data_out : {32{1'bx}} );
    bufif1 buf_dq [31:0]  (dq_out, dq_out_buf, {{32 - DQ_BITS{1'b0}} , {DQ_BITS{data_out_enable}} & by_out});

    // delayed signals
    wire clk2waite;
    wire adv2dqe;
    wire adv2dq;
    wire adv2wi;
    wire zz2pd;
    wire ce2dqe;
    wire ce2dq;
    wire ce2cem;
    wire ce2wi;
    wire ce2waite;
    wire ce2wait;
    wire ce2rst;
    wire oe2dqe;
    wire oe2dq;
    wire soe2dq;
    wire oe2waite;
    wire oe2wait;
    wire we2dqe;
    wire we2dq;
    wire we2waite;
    wire by2dqe;
    wire by2dq;
    wire addr2dq;
    wire addr2wi;
    wire saddr2dq;
    wire variable_sync;

    // Internal wires
    wire #1 last_clk   = clk_in;
    wire #1 last_adv_n = adv_n_in;
    wire #1 last_oe_n  = oe_n;
    wire #1 last_ce_n  = ce_n;
    wire #1 last_ce2wi = ce2wi;
    wire #1 last_we_n  = we_n;
    wire #1 last_cre   = cre_in;
    wire   [BY_BITS-1:0] #1 last_by_n = by_n;
    wire   [DQ_BITS-1:0] #1 last_dq   = dq_in;

    // Initial condition
    initial begin
        $timeformat (-9, 3, " ns", 1);
        sync_access <= 1'b0;
        software_access_rcr_write = 1'b0;
        async_wr_lockout = 0;
        ref_collision <= 0;
        tm_power_up <= 0.0;
        register_write(BCR, BCR_DEFAULT, 1'b0);
        register_write(RCR, RCR_DEFAULT, 1'b0);
        memory_used = 0;
    end

    always @(ce_n) begin
        if (!ce_n) begin
            if (DEBUG[0])
                $display("===============================================================");
            wait_out <= wait_polarity;
        end else begin
            if (!rbc && (sync_latency_cntr > 0)) begin
                memory_write(sync_addr, {BY_BITS{1'b0}}, {DQ_BITS{1'bx}});
                $display ("%t ERROR: Data at address 'h%h has been corrupted because CE# was deasserted prior to the first data transfer.", $realtime, sync_addr);
            end
            last_ce <= 1'b1;
            sync_latency_cntr <= 0;
            random_delay <= 0;
            row_count <= 0;
            rbc <= 0;
        end

        // Deep power-down is enabled by setting RCR[4] = 0 and taking CE# HIGH.
        if (!last_ce_n && ce_n) begin
            if (!deep_power_down_n) begin
                if (DEBUG[0])
                    $display ("%t  INFO: Async - Deep Power Down Entry", $realtime);
                memory_used = 0;
            end
            if (deep_power_down_exit) begin
                if ($realtime - tm_tdpdx < tDPDX)
                    $display ("%t ERROR: tDPDX violation on CE# by %t", $realtime, tm_tdpdx + tDPDX - $realtime);
                deep_power_down_exit <= 1'b0;
                tm_power_up <= $realtime;
            end
        end else if (last_ce_n && !ce_n) begin
            // CR1.0 RCR[4] manually disabled
            // CR1.5 Taking CE# LOW disables DPD and sets RCR[4] = 1
            if ((GENERATION > CR10) && !deep_power_down_n) begin
                register_write(RCR, ref_conf_register | 16'h0010, 1'b0);
            end
        end
    end


    // Async address latch
    always @(adv_n_in or addr) begin
        if (!adv_n_in) begin
            async_addr <= addr;
            tm_async_addr <= $realtime;
        end
    end
    always @(adv_n_in or addr[ADDR_BITS-1:4]) begin
        if (!adv_n_in) begin
            tm_upper_addr <= $realtime;
        end
    end

    // Async cre latch
    always @(adv_n_in or cre_in) begin
        if (!adv_n_in) begin
            async_cre <= cre_in;
            tm_async_cre <= $realtime;
        end
    end

    cellram_pathdelay cellram_pathdelay0 (
        clk_in,
        adv_n_in,
        zz_n_in,
        ce_n,
        oe_n,
        we_n,
        by_n,
        async_addr,
        clk2waite,
        adv2dqe,
        adv2dq,
        adv2wi,
        zz2pd,
        ce2dqe,
        ce2dq,
        ce2cem,
        ce2wi,
        ce2waite,
        ce2wait,
        ce2rst,
        oe2dqe,
        oe2dq,
        soe2dq,
        oe2waite,
        oe2wait,
        we2dqe,
        we2dq,
        we2waite,
        by2dqe,
        by2dq,
        addr2dq,
        addr2wi,
        saddr2dq
    );

    assign data_out_enable = ce2dqe && oe2dqe && we2dqe && by2dqe;
    assign variable_sync   = !operating_mode && !initial_latency && sync_access; // operating_mode == sync, initial_latency == variable
    assign data_out_valid  = we2dq && ((!variable_sync && ce2dq && oe2dq && by2dq && adv2dq && addr2dq) || (variable_sync && soe2dq && saddr2dq));
    assign wait_out_enable = cr20wait_code ? ce2waite && (oe2waite || clk2waite) && we2waite : ce2waite;
    assign wait_out_valid  = cr20wait_code ? ce2wait  && (oe2wait  || clk2waite) : ce2wait;

    always @(posedge clk) sync_access <= !ce_n;
    always @(negedge adv_n or posedge ce_n) begin
        sync_access <= 1'b0; // a burst is terminated by bringing CE# HIGH for greater than 15ns.
    end

    // Main Asynchronous block
    always @(sync_access or ce2wi or we_n or oe_n or by_n or zz_n_in or async_cre or async_addr) begin
        if (sync_access) begin
            software_access_unlock <= 2'b0;
            async_state = IDLE;
        end else begin

            // commit write to memory
            if ((async_state == WR) && ((last_ce2wi && !ce2wi) || (!last_we_n && we_n) || |(~last_by_n & by_n))) begin
                if ($realtime - tm_tcw < tCW)
                    $display ("%t ERROR:   tCW violation on CE# by %t", $realtime, tm_tcw + tCW - $realtime);
                if ($realtime - tm_adv_n_neg < tVS)
                    $display ("%t ERROR:   tVS violation on ADV# by %t", $realtime, tm_adv_n_neg + tVS - $realtime);
                if ($realtime - tm_tbw < tBW)
                    $display ("%t ERROR:   tBW violation on BY# by %t", $realtime, tm_tbw + tBW - $realtime);
                if ($realtime - tm_we_n < tWP)
                    $display ("%t ERROR:   tWP violation on WE# by %t", $realtime, tm_we_n + tWP - $realtime);
                if ($realtime - tm_async_cre < tAW)
                    $display ("%t ERROR:   tAW violation on CRE by %t", $realtime, tm_async_cre + tAW - $realtime);
                if ($realtime - tm_async_addr < tAW)
                    $display ("%t ERROR:   tAW violation on ADDR by %t", $realtime, tm_async_addr + tAW - $realtime);
                if ($realtime - tm_dq_tdw < tDW)
                    $display ("%t ERROR:   tDW violation on DQ by %t", $realtime, tm_dq_tdw + tDW - $realtime);
                if (($realtime - tm_async_addr < tWC) || ($realtime - tm_tcw < tWC))
                    $display ("%t ERROR:   tWC violation on ADDR", $realtime);

                if ((async_addr == {ADDR_BITS{1'b1}}) && (software_access_unlock == 2)) begin
                    case (last_dq[1:0])
                       (2'b00 & RCR_MASK[17:16])  : software_access_which_reg <= RCR;
                       (2'b10 & DIDR_MASK[17:16]) : software_access_which_reg <= DIDR;
                       (2'b01 & BCR_MASK[17:16])  : software_access_which_reg <= BCR;
                       default: $display ("%t ERROR: Async - Illegal Register Select = %h", $realtime, last_dq[1:0]);
                    endcase
                    if (DEBUG[0])
                        $display ("%t  INFO: Async - Software Access Unlock = %d, Register Select = %h", $realtime, software_access_unlock, last_dq[1:0]);
                    software_access_unlock <= software_access_unlock + 1;
                end else if ((async_addr == {ADDR_BITS{1'b1}}) && (software_access_unlock == 3)) begin
                    if (DEBUG[0])
                        $display ("%t  INFO: Async - Software Access Write, Data = %h", $realtime, last_dq[15:0]);
                    register_write(software_access_which_reg, last_dq[15:0], 1'b1);
                    software_access_unlock <= 2'b0;
                end else begin
                    memory_write(async_addr, last_by_n, last_dq);
                    software_access_unlock <= 2'b0;
                    if (DEBUG[0])
                        $display ("%t  INFO: Async - Write data latched, addr = %h, Mask = %h, Data = %h", $realtime, async_addr, last_by_n, last_dq);
                end
                async_state = IDLE;
                async_wr_lockout = 1;
                tavh_check <= #tAVH 1'b0;
                tow_check <= #tOW 1'b1;
            end

            // commit config write
            if ((async_state == CFG_WR) && ((last_ce2wi && !ce2wi) || (!last_we_n && we_n))) begin
                if ($realtime - tm_tcw < tCW)
                    $display ("%t ERROR:   tCW violation on CE# by %t", $realtime, tm_tcw + tCW - $realtime);
                if ($realtime - tm_adv_n_neg < tVS)
                    $display ("%t ERROR:   tVS violation on ADV# by %t", $realtime, tm_adv_n_neg + tVS - $realtime);
                if ($realtime - tm_tbw < tBW)
                    $display ("%t ERROR:   tBW violation on BY# by %t", $realtime, tm_tbw + tBW - $realtime);
                if ($realtime - tm_we_n < tWP)
                    $display ("%t ERROR:   tWP violation on WE# by %t", $realtime, tm_we_n + tWP - $realtime);
                if ($realtime - tm_async_cre < tAW)
                    $display ("%t ERROR:   tAW violation on CRE by %t", $realtime, tm_async_cre + tAW - $realtime);
                if ($realtime - tm_async_addr < tAW)
                    $display ("%t ERROR:   tAW violation on ADDR by %t", $realtime, tm_async_addr + tAW - $realtime);
                if ($realtime - tm_dq_tdw < tDW)
                    $display ("%t ERROR:   tDW violation on DQ by %t", $realtime, tm_dq_tdw + tDW - $realtime);
                if (($realtime - tm_async_addr < tWC) || ($realtime - tm_tcw < tWC))
                    $display ("%t ERROR:   tWC violation on ADDR", $realtime);

                if (DEBUG[0])
                    $display ("%t  INFO: Async - Configuration Register Write data latched, Register Select = %h, Data = %h", $realtime, async_addr>>REG_SEL, async_addr);
                register_write(async_addr>>REG_SEL, async_addr[15:0], 1'b1);
                software_access_unlock <= 2'b0;
                async_state = IDLE;
                async_wr_lockout = 1;
                tow_check <= #tOW 1'b1;
                tavh_check <= #tAVH 1'b0;
                tm_write_end = $realtime;
           end

            // end read cycle
            if (async_state == RD) begin
                if (last_ce2wi && !ce2wi) begin
                    if (($realtime - tm_upper_addr < tRC) || ($realtime - tm_tcw < tRC))
                        $display ("%t ERROR:   tRC violation on ADDR", $realtime);
                    if ($realtime - tm_page_addr3 < tPC) $display ("%t ERROR:   tPC violation on ADDR bit %d by %t", $realtime, i, tm_page_addr3 + tPC - $realtime);
                    if ($realtime - tm_page_addr2 < tPC) $display ("%t ERROR:   tPC violation on ADDR bit %d by %t", $realtime, i, tm_page_addr2 + tPC - $realtime);
                    if ($realtime - tm_page_addr1 < tPC) $display ("%t ERROR:   tPC violation on ADDR bit %d by %t", $realtime, i, tm_page_addr1 + tPC - $realtime);
                    if ($realtime - tm_page_addr0 < tPC) $display ("%t ERROR:   tPC violation on ADDR bit %d by %t", $realtime, i, tm_page_addr0 + tPC - $realtime);
                    async_state = IDLE;
                end else if (!last_oe_n && oe_n) begin
                    async_state = IDLE;
                end
            end

            // new write command won't be decoded until WE# goes high
            if (!last_we_n && we_n) begin
                async_wr_lockout = 0;
            end
            // check tAVH until tAVH after CE# deasserts
            if (last_ce2wi && !ce2wi) begin
                tavh_check <= #tAVH 1'b0;
            end

            // command decode
            if (!async_wr_lockout && ce2wi && !we_n && !async_cre) begin
                if (async_state == WR) begin
                    $display ("%t ERROR: Async - Illegal or unintended Page Mode Write.", $realtime);
                end
                async_state = WR;
                last_access <= 1'b0;
                last_ce <= 1'b0;
                tavh_check <= 1'b1;
                if (DEBUG[0])
                    $display ("%t  INFO: Async - Write", $realtime);
            end
            if (!async_wr_lockout && ce2wi && !we_n && (async_cre || !zz_n_in) && ((GENERATION != CR20) || oe_n)) begin
                if (async_state == CFG_WR) begin
                    $display ("%t ERROR: Async - Page Mode Configuration Register Write is Illegal.", $realtime);
                end
                async_state = CFG_WR;
                last_access <= 1'b0;
                last_ce <= 1'b0;
                tavh_check <= 1'b1;
                if (DEBUG[0])
                    $display ("%t  INFO: Async - Configuration Register Write", $realtime);
            end
            if (ce2wi && !oe_n && we_n && !async_cre) begin
                next_data_out = {DQ_BITS{1'bx}};
                if (!page_mode && (async_state == RD))
                    $display ("%t ERROR: Async - Illegal or unintended Page Mode Read.  Page mode is disabled in BCR", $realtime);
                if ((GENERATION > CR10) && (by_n !== {BY_BITS{1'b0}}))
                    $display ("%t ERROR: Async - BY# must be LOW during READ cycles.", $realtime);
                if (async_addr == {ADDR_BITS{1'b1}} && (software_access_unlock == 3)) begin
                    case (software_access_which_reg)
                        RCR  : next_data_out = ref_conf_register | (RCR<<REG_SEL);
                        DIDR : next_data_out = didr_conf_register | (DIDR<<REG_SEL);
                        BCR  : next_data_out = bus_conf_register | (BCR<<REG_SEL);
                    endcase
                    if (DEBUG[0])
                        $display ("%t  INFO: Async - Software Access Read, Data = %h", $realtime, next_data_out);
                    software_access_unlock <= 2'b0;
                end else begin
                    next_data_out = memory_read(async_addr);
                    if (DEBUG[0])
                        $display ("%t  INFO: Async - Read, addr = %h, Data = %h", $realtime, async_addr, next_data_out);
                    if (async_addr == {ADDR_BITS{1'b1}}) begin
                        if (software_access_unlock < 2) begin
                            if (DEBUG[0])
                                $display ("%t  INFO: Async - Software Access Unlock = %d", $realtime, software_access_unlock);
                            software_access_unlock <= software_access_unlock + 1;
                        end
                    end else begin
                        software_access_unlock <= 2'b0;
                    end
                end
                data_out <= #tOH {DQ_BITS{1'bx}};
                if (async_state == RD) begin
                    data_out <= #tAPA next_data_out;
                end else begin
                    data_out <= #tOE next_data_out;
                end
                async_state = RD;
                last_access <= 1'b0;
                last_ce <= 1'b0;
                tavh_check <= 1'b1;
            end
            if (ce2wi && !oe_n && we_n && async_cre) begin
                next_data_out = {DQ_BITS{1'bx}};
                if (async_state == RD)
                    $display ("%t ERROR: Async - Page Mode Configuration Register Read is Illegal.", $realtime);
                if ((GENERATION > CR10) && (by_n !== {BY_BITS{1'b0}}))
                    $display ("%t ERROR: Async - BY# must be LOW during READ cycles.", $realtime);
                if (!CRE_READ)
                    $display ("%t ERROR: Async - Access using CRE is WRITE only.", $realtime);
                case (async_addr>>REG_SEL)
                    (RCR  & RCR_MASK[17:16])  : next_data_out = ref_conf_register | (RCR<<REG_SEL);
                    (DIDR & DIDR_MASK[17:16]) : next_data_out = didr_conf_register | (DIDR<<REG_SEL);
                    (BCR  & BCR_MASK[17:16])  : next_data_out = bus_conf_register | (BCR<<REG_SEL);
                    default: $display ("%t ERROR: Async - Illegal Register Select = %h", $realtime, async_addr>>REG_SEL);
                endcase
                if (DEBUG[0])
                    $display ("%t  INFO: Async - Configuration Register Read, Register Select = %h, Data = %h", $realtime, async_addr>>REG_SEL, next_data_out);
                software_access_unlock <= 2'b0;
                data_out <= #tOH {DQ_BITS{1'bx}};
                data_out <= #tOE next_data_out;
                async_state = RD;
                last_access <= 1'b0;
                last_ce <= 1'b0;
                tavh_check <= 1'b1;
            end

            if (ce2wi) begin
                next_by_out = 0;
                for (i=0; i<BY_BITS; i=i+1) begin
                    next_by_out = next_by_out | ({`DQ_PER_BY{!by_n[i]}}<<(i*`DQ_PER_BY));
                end
                by_out <= #tOH next_by_out;
            end
        end
    end

    always @(par_enabled or partial_address_offset or partial_address_size) begin : corrupt_memory
        integer i;
        if (par_enabled) begin
            i = 0;
            // remove the selected addresses
            if (partial_address_size > 0) begin
                for (memory_index=0; memory_index<memory_used; memory_index=memory_index+1) begin
                    if ((memory_addr[memory_index] > partial_address_offset) && (memory_addr[memory_index] < partial_address_offset + partial_address_size)) begin
                        memory_addr[i] = memory_addr[memory_index];
                        memory[i] = memory[memory_index];
                        i = i + 1;
                    end
                end
            end
            // clean up the unused addresses
            for (memory_index=i; memory_index<memory_used; memory_index=memory_index+1) begin
                memory_addr[memory_index] = 'bx;
                memory[memory_index] = {DQ_BITS{1'bx}};
            end
            memory_used = i;
        end
    end

    function memory_addr_exists;
        input [ADDR_BITS-1:0] addr;
        begin : index
            memory_addr_exists = 0;
            for (memory_index=0; memory_index<memory_used; memory_index=memory_index+1) begin
                if (memory_addr[memory_index] == addr) begin
                    memory_addr_exists = 1;
                    disable index;
                end
            end
        end
    endfunction

    task memory_write;
        input [ADDR_BITS-1:0] addr;
        input [BY_BITS-1:0] data_mask;
        input [DQ_BITS-1:0] data;

        reg [DQ_BITS-1:0] bit_mask;
        reg [DQ_BITS-1:0] write_data;
        integer i;
        begin : memory_write_task
            if (par_enabled && (partial_address_size == 0)) begin
                $display ("%t  WARN: Illegal Address = %h.  Address range is NULL", $realtime, addr);
                disable memory_write_task; // end task - nothing is written
            end else if (par_enabled && ((addr < partial_address_offset) || (addr > partial_address_offset + partial_address_size - 1))) begin
                $display ("%t  WARN: Illegal Address = %h.  Address range is %h - %h", $realtime, addr, partial_address_offset, partial_address_offset + partial_address_size - 1);
                disable memory_write_task; // end task - nothing is written
            end else if (addr > (1<<ADDR_BITS) - 1) begin
                $display ("%t  WARN: Illegal Address = %h.  Max Address is %h", $realtime, addr, (1<<ADDR_BITS) - 1);
                disable memory_write_task; // end task - nothing is written
            end else if (data_mask == {BY_BITS{1'b1}}) begin
                disable memory_write_task; // end task - nothing is written
            end else if (data_mask == {BY_BITS{1'b0}}) begin
                write_data = {DQ_BITS{1'b1}} & data;
            end else begin
                bit_mask = 0;
                for (i=0; i<BY_BITS; i=i+1) begin
                    bit_mask = bit_mask | ({`DQ_PER_BY{!data_mask[i]}}<<(i*`DQ_PER_BY));
                end
                write_data = (data & bit_mask) | (memory_read(addr) & ~bit_mask);
            end

            if (memory_used == 1<<MEM_BITS) begin
                $display ("%t ERROR: Memory overflow.  Write to Address %h with Data %h will be lost.\nYou must increase the MEM_BITS parameter.", $realtime, addr, data);
                $stop(0);
            end else begin
                if (!memory_addr_exists(addr)) begin
                    memory_used = memory_used + 1;
                end
                memory_addr[memory_index] = addr;
                memory[memory_index] = write_data;
            end
        end
    endtask

    function [DQ_BITS-1:0] memory_read;
        input [ADDR_BITS-1:0] addr;
        begin
            if (partial_address_size == 0) begin
                $display ("%t  WARN: Illegal Address = %h.  Address range is NULL", $realtime, addr);
                memory_read = {DQ_BITS{1'bx}};
            end else if ((addr < partial_address_offset) || (addr > partial_address_offset + partial_address_size - 1)) begin
                $display ("%t  WARN: Illegal Address = %h.  Address range is %h - %h", $realtime, addr, partial_address_offset, partial_address_offset + partial_address_size - 1);
                memory_read = {DQ_BITS{1'bx}};
            end else begin
                if (memory_addr_exists(addr)) begin
                    memory_read = memory[memory_index];
                end else begin
                    memory_read = {DQ_BITS{1'bx}};
                end
            end
        end
    endfunction

    task register_write;
        input  [1:0] select;
        input [15:0] opcode;
        input        report;
        begin

            case(select)
                BCR: begin
                    burst_length        = opcode[2:0];
                    burst_wrap_n        = opcode[3];
                    drive_strength      = opcode[5:4];
                    clock_configuration = opcode[6];
                    wait_configuration  = opcode[8];
                    cr20wait            = opcode[9];
                    cr20wait_code       = (cr20wait == CR20WAIT_POLARITY);
                    wait_polarity       = opcode[10];
                    latency_counter     = (opcode[13:11] == 3'b000) ? 4'd8 : opcode[13:11];
                    initial_latency     = opcode[14];
                    if (!operating_mode && opcode[15]) begin // turning on async mode
                        Sync2AsyncConfig = latency_counter;
                    end
                    operating_mode      = opcode[15];
                end
                RCR: begin
                    partial_array_refresh = opcode[2:0];
                    if (software_access_unlock) begin
                        if ((GENERATION == CR10) && (deep_power_down_n ^ opcode[4]))
                            $display ("%t ERROR: Deep Power Down cannot be enabled or disabled using the software access sequence", $realtime);
                    end
                    // exit deep power down
                    if (!deep_power_down_n && opcode[4]) begin
                        if (GENERATION == CR10) begin
                            tm_power_up <= $realtime;
                        end else begin
                            if ($realtime - tm_tdpd < tDPD)
                                $display ("%t ERROR:  tDPD violation on CE# by %t", $realtime, tm_tdpd + tDPD - $realtime);
                            deep_power_down_exit <= 1'b1;
                            tm_tdpdx <= $realtime;
                        end
                        if (DEBUG[0])
                            $display ("%t  INFO: Deep Power Down Exit", $realtime);
                    end
                    deep_power_down_n     = opcode[4];
                    case_temp             = opcode[6:5];
                    page_mode             = opcode[7];
                end
            endcase

            if (report) begin
                case(select)
                    (BCR & BCR_MASK[17:16]): begin
                        $display ("%t  INFO: Register Select = BCR", $realtime);
                        if ((~BCR_MASK & opcode) !== (~BCR_MASK & BCR_DEFAULT))
                            $display ("%t ERROR: Reserved bits in BCR cannot be changed from their default value.", $realtime);

                        if (&BCR_MASK[2:0]) begin
                            casex ({(GENERATION > CR10), burst_length})
                                4'bx001:$display ("%t  INFO: Burst Length =  4 words", $realtime);
                                4'bx010:$display ("%t  INFO: Burst Length =  8 words", $realtime);
                                4'bx011:$display ("%t  INFO: Burst Length = 16 words", $realtime);
                                4'b1100:$display ("%t  INFO: Burst Length = 32 words", $realtime);
                                4'bx111:$display ("%t  INFO: Burst Length = continuous", $realtime);  // default
                                default:$display ("%t ERROR: Illegal Burst Length = %h", $realtime, burst_length);
                            endcase
                        end

                        if (BCR_MASK[3]) begin
                            case (burst_wrap_n)
                                1'b0   :$display ("%t  INFO: Burst Wrap = Burst wraps within the burst length", $realtime);
                                1'b1   :$display ("%t  INFO: Burst Wrap = Burst no wrap", $realtime);  // default
                                default:$display ("%t ERROR: Illegal Burst Wrap = %h", $realtime, burst_wrap_n);
                            endcase
                        end

                        if (|BCR_MASK[5:4]) begin
                            case (drive_strength & BCR_MASK[5:4])
                                2'b00  :$display ("%t  INFO: Drive Strength = Full", $realtime);  // default
                                2'b01  :$display ("%t  INFO: Drive Strength = 1/2", $realtime);
                                2'b10  :$display ("%t  INFO: Drive Strength = 1/4", $realtime);
                                default:$display ("%t ERROR: Illegal Drive Strength = %h", $realtime, drive_strength);
                            endcase
                        end

                        if (BCR_MASK[6]) begin
                            case (clock_configuration)
                                1'b1   :$display ("%t  INFO: Clock Configuration = Rising edge", $realtime); // default
                                default:$display ("%t ERROR: Illegal Clock Configuration = %h", $realtime, clock_configuration);
                            endcase
                        end

                        if (BCR_MASK[8]) begin
                            case (wait_configuration)
                                1'b0   :$display ("%t  INFO: WAIT Configuration = Asserted during delay", $realtime);
                                1'b1   :$display ("%t  INFO: WAIT Configuration = Asserted one data cycle before delay", $realtime);  // default
                                default:$display ("%t ERROR: Illegal WAIT Configuration = %h", $realtime, wait_configuration);
                            endcase
                        end

                        if (BCR_MASK[9]) begin
                            case (cr20wait_code)
                                1'b0   :$display ("%t  INFO: CR1.5 WAIT Behavior", $realtime);
                                1'b1   :$display ("%t  INFO: CR2.0 WAIT Behavior", $realtime);
                                default:$display ("%t ERROR: Illegal CR2.0 WAIT Behavior = %h", $realtime, cr20wait_code);
                            endcase
                        end

                        if (BCR_MASK[10]) begin
                            case (wait_polarity)
                                1'b0   :$display ("%t  INFO: Wait Polarity = Active LOW", $realtime);
                                1'b1   :$display ("%t  INFO: Wait Polarity = Active HIGH", $realtime);  // default
                                default:$display ("%t ERROR: Illegal Wait Polarity = %h", $realtime, wait_polarity);
                            endcase
                        end

                        if (&BCR_MASK[13:11]) begin
                            casex ({initial_latency, latency_counter})
                                5'hx2  :$display ("%t  INFO: Latency Counter = Code 2", $realtime);
                                5'hx3  :$display ("%t  INFO: Latency Counter = Code 3", $realtime);   // default
                                5'h14  :$display ("%t  INFO: Latency Counter = Code 4", $realtime);
                                5'h15  :$display ("%t  INFO: Latency Counter = Code 5", $realtime);
                                5'h16  :$display ("%t  INFO: Latency Counter = Code 6", $realtime);
                                5'h18  :$display ("%t  INFO: Latency Counter = Code 8", $realtime);
                                default:$display ("%t ERROR: Illegal Latency Counter = %h", $realtime, latency_counter);
                            endcase
                        end

                        if (BCR_MASK[14]) begin
                            case (initial_latency)
                                1'b0   :$display ("%t  INFO: Initial Access Latency = Variable", $realtime);  // default
                                1'b1   :$display ("%t  INFO: Initial Access Latency = Fixed", $realtime);
                                default:$display ("%t ERROR: Illegal Initial Access Latency = %h", $realtime, initial_latency);
                            endcase
                        end

                        if (BCR_MASK[15]) begin
                            case (operating_mode)
                                1'b0   :$display ("%t  INFO: Operating Mode = Synchronous burst access mode", $realtime);
                                1'b1   :$display ("%t  INFO: Operating Mode = Asynchronous access mode", $realtime);  //default
                                default:$display ("%t ERROR: Illegal Operating Mode = %h", $realtime, operating_mode);
                            endcase
                        end
                    end

                    (RCR & RCR_MASK[17:16]): begin
                        $display ("%t  INFO: Register Select = RCR", $realtime);
                        if ((~RCR_MASK & opcode) !== (~RCR_MASK & RCR_DEFAULT))
                            $display ("%t ERROR: Reserved bits in RCR cannot be changed from their default value.", $realtime);

                        if (RCR_MASK[2:0]) begin
                            case (partial_array_refresh)
                               3'b000 : begin $display ("%t  INFO: Partial Array Refresh =       Full array", $realtime); partial_address_size = 1<<(ADDR_BITS-0); partial_address_offset = 0; end
                               3'b001 : begin $display ("%t  INFO: Partial Array Refresh = Bottom 1/2 array", $realtime); partial_address_size = 1<<(ADDR_BITS-1); partial_address_offset = 0; end
                               3'b010 : begin $display ("%t  INFO: Partial Array Refresh = Bottom 1/4 array", $realtime); partial_address_size = 1<<(ADDR_BITS-2); partial_address_offset = 0; end
                               3'b011 : begin $display ("%t  INFO: Partial Array Refresh = Bottom 1/8 array", $realtime); partial_address_size = 1<<(ADDR_BITS-3); partial_address_offset = 0; end
                               3'b100 : begin $display ("%t  INFO: Partial Array Refresh =             None", $realtime); partial_address_size = 0;                partial_address_offset = 0; end
                               3'b101 : begin $display ("%t  INFO: Partial Array Refresh =    Top 1/2 array", $realtime); partial_address_size = 1<<(ADDR_BITS-1); partial_address_offset = (1<<ADDR_BITS) - partial_address_size; end
                               3'b110 : begin $display ("%t  INFO: Partial Array Refresh =    Top 1/4 array", $realtime); partial_address_size = 1<<(ADDR_BITS-2); partial_address_offset = (1<<ADDR_BITS) - partial_address_size; end
                               3'b111 : begin $display ("%t  INFO: Partial Array Refresh =    Top 1/8 array", $realtime); partial_address_size = 1<<(ADDR_BITS-3); partial_address_offset = (1<<ADDR_BITS) - partial_address_size; end
                               default: begin $display ("%t ERROR: Illegal Partial Array Refresh = %h", $realtime, partial_array_refresh); end
                            endcase
                        end

                        if (RCR_MASK[4]) begin
                            case (deep_power_down_n)
                               1'b0   : $display ("%t  INFO: Deep Power Down = DPD Enable", $realtime);
                               1'b1   : $display ("%t  INFO: Deep Power Down = DPD Disable", $realtime);
                               default: $display ("%t ERROR: Illegal Deep Power Down = %h", $realtime, deep_power_down_n);
                            endcase
                        end

                        if (&RCR_MASK[6:5]) begin
                            case (case_temp)
                               2'b00  : $display ("%t  INFO: Maximum Case Temp = 70 degrees C", $realtime);
                               2'b01  : $display ("%t  INFO: Maximum Case Temp = 45 degrees C", $realtime);
                               2'b10  : $display ("%t  INFO: Maximum Case Temp = 15 degrees C", $realtime);
                               2'b11  : $display ("%t  INFO: Maximum Case Temp = 85 degrees C", $realtime);
                               default: $display ("%t ERROR: Illegal Maximum Case Temp = %h", $realtime, case_temp);
                            endcase
                        end

                        if (RCR_MASK[7]) begin
                            case (page_mode)
                               1'b0   : $display ("%t  INFO: Page Mode = Page Mode Disabled", $realtime);
                               1'b1   : $display ("%t  INFO: Page Mode = Page Mode Enable", $realtime);
                               default: $display ("%t ERROR: Illegal Page Mode = %h", $realtime, page_mode);
                            endcase
                        end
                    end
                    (DIDR & DIDR_MASK[17:16]): begin
                        $display ("%t  INFO: Register Select = DIDR", $realtime);
                        $display ("%t ERROR: DIDR Configuration Register is read-only", $realtime);
                    end
                    default: begin
                        $display ("%t ERROR: Illegal Register Select = %d", $realtime, select);
                    end
                endcase
            end
        end
    endtask

    // Main Synchronous block
    always @(posedge clk_in) begin
        if (!operating_mode || Sync2AsyncConfig) begin // Sync

            if (!ce_n) begin
                if (!adv_n_in) begin // latch address and command
                    sync_addr = addr;
                    // CR2.0 ADQ[30] = continuous burst mode.
                    sync_burst_length_override = (dq_in[DQ_BITS-2] && (GENERATION == CR20));
                    sync_cre = cre_in;
                    sync_we_n = we_n;

                    if (!rbc && (sync_latency_cntr > 0))
                        $display ("%t ERROR: Illegal burst interrupt.  Burst interrupt is illegal until after the first data word has been transferred.", $realtime);
                    last_access <= 1'b1;
                    last_ce <= 1'b0;
                    random_delay <= 0;
                    row_count <= 0;
                    rbc <= 0;

                    if (!sync_we_n) begin // write
                        if (DEBUG[0]) begin
                            if (sync_cre) begin
                                $display ("%t  INFO:  Sync - Configuration Register Write", $realtime);
                            end else begin
                                $display ("%t  INFO:  Sync - Write", $realtime);
                            end
                        end
                    end else begin //read
                        if (DEBUG[0]) begin
                            if (sync_cre) begin
                                $display ("%t  INFO:  Sync - Configuration Register Read", $realtime);
                            end else begin
                                $display ("%t  INFO:  Sync - Read", $realtime);
                            end
                        end
                        // variable latency
                        if (!initial_latency && ($time - ref_collision >= tCEM)) begin
                            ref_collision <= $time;
                            random_delay = ($random % latency_counter);
                            if (DEBUG[0] && (random_delay > 0))
                                $display ("%t  INFO:  Sync - Read refresh collision of %d clocks has occurred.", $realtime, random_delay);
                        end
                    end
                    sync_latency_cntr = !sync_we_n + random_delay + latency_counter + 1;
                    sync_wait = wait_polarity;
                    sync_wait_q = wait_polarity;
                end

                if (!sync_we_n) begin
                    if ((!sync_cre) && (oe_n !== 1))
                        $display ("%t ERROR: Sync - OE# must be driven high during sync writes", $realtime);
                end else begin
                    if ((GENERATION > CR10) && (by_n !== {BY_BITS{1'b0}}))
                        $display ("%t ERROR: Sync - BY# must be LOW during READ cycles.", $realtime);
                end

                if (row_count > 0) begin
                    if (row_count == 1) begin
                        $display ("%t ERROR: CE# must go HIGH before the third CLK after the WAIT period asserts with BCR[8] = 0, or before the fourth CLK after WAIT asserts with BCR[8] = 1", $realtime);
                    end else begin
                        row_count = row_count - 1;
                    end
                end else if (rbc) begin
                    if (sync_latency_cntr == 2*latency_counter + 2 + !sync_we_n) begin
                        rbc_by = by_n;
                        rbc_dq = dq_in;
                    end else if (sync_latency_cntr == 2) begin
                        if (!sync_we_n) begin
                            if (DEBUG[0])
                                $display ("%t  INFO:  Sync - Write, addr = %h, Mask = %h, Data = %h - Row Boundary Crossing", $realtime, sync_addr, rbc_by, rbc_dq);
                            memory_write(sync_addr, rbc_by, rbc_dq);
                            sync_addr[COL_BITS-1:0] = sync_addr[COL_BITS-1:0] + 1;
                        end
                        rbc <= 0;
                    end
                end else if (sync_latency_cntr < 2) begin // burst continue
                    next_by_out = 0;
                    for (i=0; i<BY_BITS; i=i+1) begin
                        next_by_out = next_by_out | ({`DQ_PER_BY{!by_n[i]}}<<(i*`DQ_PER_BY));
                    end
                    by_out   <= #tKOH next_by_out;
                    data_out <= #tKOH {DQ_BITS{1'bx}};

                    if (!sync_we_n) begin // write
                        if (sync_cre) begin // write config
                            if (DEBUG[0])
                                $display ("%t  INFO:  Sync - Configuration Register Write data latched, Register Select = %h, Data = %h", $realtime, sync_addr>>REG_SEL, sync_addr);
                            register_write(sync_addr>>REG_SEL, sync_addr[15:0], 1'b1);
                        end else begin // write array
                            if (DEBUG[0])
                                $display ("%t  INFO:  Sync - Write, addr = %h, Mask = %h, Data = %h", $realtime, sync_addr, by_n, dq_in);
                            memory_write(sync_addr, by_n, dq_in);
                        end
                    end else begin // read
                        if (sync_cre) begin // read config
                            case (sync_addr>>REG_SEL)
                                (RCR  & RCR_MASK[17:16])  : next_data_out = ref_conf_register | (RCR<<REG_SEL);
                                (DIDR & DIDR_MASK[17:16]) : next_data_out = didr_conf_register | (DIDR<<REG_SEL);
                                (BCR  & BCR_MASK[17:16])  : next_data_out = bus_conf_register | (BCR<<REG_SEL);
                                default: $display ("%t ERROR:  Sync - Illegal Register Select = %h", $realtime, sync_addr>>REG_SEL);
                            endcase
                            if (DEBUG[0])
                                $display ("%t  INFO:  Sync - Configuration Register Read, Register Select = %h, Data = %h", $realtime, sync_addr>>REG_SEL, next_data_out);
                            data_out <= #tACLK next_data_out;
                        end else begin // read array
                            next_data_out = memory_read(sync_addr);
                            if (DEBUG[0])
                                $display ("%t  INFO:  Sync - Read, addr = %h, Data = %h", $realtime, sync_addr, next_data_out);
                            data_out <= #tACLK next_data_out;
                        end
                    end

                    // increment address
                    if (!sync_cre) begin
                        // CR1.0 Burst wrap and length apply to READ operations
                        // CR1.5 Burst wrap and length apply to READ and WRITE operations
                        if (!burst_wrap_n && (sync_we_n || (GENERATION > CR10))) begin // Burst Wrap
                            case (burst_length | {3{sync_burst_length_override}}) // burst length or burst override
                                3'b001 : sync_addr[         1:0] = sync_addr[         1:0] + 1;
                                3'b010 : sync_addr[         2:0] = sync_addr[         2:0] + 1;
                                3'b011 : sync_addr[         3:0] = sync_addr[         3:0] + 1;
                                3'b100 : sync_addr[         4:0] = sync_addr[         4:0] + 1;
                                3'b111 : sync_addr[COL_BITS-1:0] = sync_addr[COL_BITS-1:0] + 1;
                            endcase
                            $display ("%t  INFO:  Sync - increment addr = %h", $realtime, sync_addr);
                        end else begin // Burst no Wrap
                            if (sync_addr[COL_BITS-1:0] == {COL_BITS{1'b1}}) begin
                                sync_wait = wait_polarity;
                                // row boundary crossing allowed in variable latency
                                if (!initial_latency && (GENERATION == CR10)) begin
                                    rbc <= 1;
                                    sync_latency_cntr = 2*latency_counter + 3 + !sync_we_n;
                                end else begin
                                    row_count = 3 + (sync_we_n | !cr20wait_code);
                                end
                            end
                            sync_addr[COL_BITS-1:0] = sync_addr[COL_BITS-1:0] + 1;
                        end
                    end
                end

                if (sync_latency_cntr == (!sync_we_n + 2)) begin
                    sync_wait = !wait_polarity;
                // CR2.0 wait will assert 1 clock earlier on a write than in CR1.5
                end else if (cr20wait_code && !sync_we_n && burst_wrap_n && (sync_addr[COL_BITS-1:0] == {COL_BITS{1'b1}})) begin
                    sync_wait = wait_polarity;
                end
            end
        end

        if (sync_latency_cntr > 0) begin
            sync_latency_cntr <= sync_latency_cntr - 1;
        end

        if (Sync2AsyncConfig > 0) begin
            Sync2AsyncConfig <= Sync2AsyncConfig - 1;
        end

        if (wait_configuration) begin // one clock early
            wait_out <= #tKHTL sync_wait;
        end else begin
            wait_out <= #tKHTL sync_wait_q;
        end

        sync_wait_q <= sync_wait;
    end


    // timing checks
    always @(posedge clk_in) begin
        if ($realtime > tCLK) begin
            if ($realtime - tm_ce_n < tCSP)
                $display ("%t ERROR:  tCSP violation on CE# by %t", $realtime, tm_ce_n + tCSP - $realtime);
            if (!ce_n) begin
                if ($realtime - tm_adv_n < tSP)
                    $display ("%t ERROR:   tSP violation on ADV# by %t", $realtime, tm_adv_n + tSP - $realtime);
                if (adv_n_in) begin
                    if ($realtime - tm_by_n < tSP)
                        $display ("%t ERROR:   tSP violation on BY# by %t", $realtime, tm_by_n + tSP - $realtime);
                end else begin
                    if ($realtime - tm_we_n < tSP)
                        $display ("%t ERROR:   tSP violation on WE# by %t", $realtime, tm_we_n + tSP - $realtime);
                    if ($realtime - tm_cre < tSP)
                        $display ("%t ERROR:   tSP violation on CRE by %t", $realtime, tm_cre + tSP - $realtime);
                    if ($realtime - tm_addr < tSP)
                        $display ("%t ERROR:   tSP violation on ADDR by %t", $realtime, tm_addr + tSP - $realtime);
                end

                tm_clk_period = min_clk_period(initial_latency, latency_counter[2:0]);
                if (tm_clk_period == 0.0)
                    $display ("%t ERROR: Illegal latency counter = %h", $realtime, latency_counter);
                if ($realtime - tm_clk_pos < tm_clk_period)
                    $display ("%t ERROR: Clock Period must be >= %f while Latency Counter = %h.  Actual Clock Period = %f", $realtime, tm_clk_period, latency_counter, $realtime - tm_clk_pos);

                if ($realtime - tm_clk_pos < tCLK)
                    $display ("%t ERROR:  tCLK minimum violation on CLK by %t", $realtime, tm_clk_pos + tCLK - $realtime);
                if ($realtime - tm_clk_neg < tKP)
                    $display ("%t ERROR:   tKP minimum violation on CLK by %t", $realtime, tm_clk_neg + tKP - $realtime);
            end
        end
        tm_clk_pos = $realtime - 0.0001;  // adjust for realtime rounding behavior
    end

    always @(negedge clk_in) begin
        if ($realtime > tCLK) begin
            if ($realtime - tm_clk_pos < tKP)
                $display ("%t ERROR:   tKP minimum violation on CLK by %t", $realtime, tm_clk_pos + tKP - $realtime);
        end
        tm_clk_neg = $realtime - 0.0001;  // adjust for realtime rounding behavior;
    end

    always @(ce_n) begin
        if ($realtime > tCLK) begin
            if ($realtime - tm_clk_pos < tHD)
                $display ("%t ERROR:   tHD violation on CE# by %t", $realtime, tm_clk_pos + tHD - $realtime);
        end
        if (!ce_n) begin
            if (last_access) begin
                if ($realtime - tm_ce_n < tCBPH)
                    $display ("%t ERROR: tCBPH violation on CE# by %t", $realtime, tm_ce_n + tCBPH - $realtime);
            end else begin
                if ($realtime - tm_ce_n < tCPH)
                    $display ("%t ERROR:  tCPH violation on CE# by %t", $realtime, tm_ce_n + tCPH - $realtime);
                if ($realtime - tm_write_end < tWR)
                    $display ("%t ERROR:   tWR violation on CE# by %t", $realtime, tm_write_end + tWR - $realtime);
            end
            if ($realtime - tm_power_up < tPU)
                $display ("%t  WARN:   tPU violation on CE# by %t", $realtime, tm_power_up + tPU - $realtime);
        end
        tm_ce_n = $realtime;
        tm_tcw  <= $realtime;
        tm_tdpd <= $realtime;
    end

    always @(adv_n_in) begin
        if (!ce_n) begin
            if ($realtime - tm_clk_pos < tHD)
                $display ("%t ERROR:   tHD violation on ADV# by %t", $realtime, tm_clk_pos + tHD - $realtime);
            if (adv_n_in) begin
                if (!sync_access) begin
                    if ($realtime - tm_ce_n < tCVS)
                        $display ("%t ERROR:  tCVS violation on CE# by %t", $realtime, tm_ce_n + tCVS - $realtime);
                    if ($realtime - tm_adv_n < tVP)
                        $display ("%t ERROR:   tVP violation on ADV# by %t", $realtime, tm_adv_n + tVP - $realtime);
                    if ($realtime - tm_cre < tAVS)
                        $display ("%t ERROR:  tAVS violation on CRE by %t", $realtime, tm_cre + tAVS - $realtime);
                    if ($realtime - tm_addr < tAVS)
                        $display ("%t ERROR:  tAVS violation on ADDR by %t", $realtime, tm_addr + tAVS - $realtime);
                end
                if (!sync_access | initial_latency) begin
                    if ($realtime - tm_cre < tAVH)
                        $display ("%t ERROR:  tAVH violation on CRE by %t", $realtime, tm_cre + tAVH - $realtime);
                    if ($realtime - tm_addr < tAVH)
                        $display ("%t ERROR:  tAVH violation on ADDR by %t", $realtime, tm_addr + tAVH - $realtime);
                end
            end else begin
                if ($realtime - tm_adv_n < tVPH)
                    $display ("%t ERROR:   tVPH violation on ADV# by %t", $realtime, tm_adv_n + tVPH - $realtime);
            end
        end
        if (adv_n_in) begin
            tm_adv_n_pos = $realtime;
        end else begin
            tm_adv_n_neg <= $realtime;
        end
        tm_adv_n = $realtime;
    end

    always @(we_n) begin
        if (!ce_n) begin
            if (!adv_n_in) begin
                if ($realtime - tm_clk_pos < tHD)
                    $display ("%t ERROR:   tHD violation on WE# by %t", $realtime, tm_clk_pos + tHD - $realtime);
            end
            if (!sync_access) begin
                if (!we_n) begin
                    if ($realtime - tm_we_n < tWPH)
                        $display ("%t ERROR:   tWPH violation on WE# by %t", $realtime, tm_we_n + tWPH - $realtime);
                end
            end
        end
        tm_we_n <= $realtime;
    end

    // check tAS
    always @(we_n or ce2wi) begin
        if (!sync_access && !we_n && ce2wi) begin
            if ($realtime - tm_adv_n_neg < tAS)
                $display ("%t ERROR:   tAS violation on ADV# by %t", $realtime, tm_adv_n_neg + tAS - $realtime);
            if ($realtime - tm_async_addr < tAS)
                $display ("%t ERROR:   tAS violation on ADDR by %t", $realtime, tm_async_addr + tAS - $realtime);
        end
    end

    always @(by_n) begin
        if (!ce_n) begin
            if (adv_n_in) begin
                if ($realtime - tm_clk_pos < tHD)
                    $display ("%t ERROR:   tHD violation on BY# by %t", $realtime, tm_clk_pos + tHD - $realtime);
            end
        end
        tm_by_n = $realtime;
        tm_tbw <= $realtime;
    end

    always @(cre_in) begin
        if (!ce_n | tavh_check) begin
            if (!adv_n_in) begin
                if ($realtime - tm_clk_pos < tHD)
                    $display ("%t ERROR:   tHD violation on CRE by %t", $realtime, tm_clk_pos + tHD - $realtime);
            end
            if (!sync_access | initial_latency) begin
                if ($realtime - tm_adv_n_pos < tAVH)
                    $display ("%t ERROR:  tAVH violation on CRE by %t", $realtime, tm_adv_n_pos + tAVH - $realtime);
            end
        end
        tm_cre = $realtime;
    end

    always @(addr) begin
        if (!ce_n | tavh_check) begin
            if (!adv_n_in) begin
                if ($realtime - tm_clk_pos < tHD)
                    $display ("%t ERROR:   tHD violation on ADDR by %t", $realtime, tm_clk_pos + tHD - $realtime);
            end
            if (!sync_access | initial_latency) begin
                if ($realtime - tm_adv_n_pos < tAVH)
                    $display ("%t ERROR:  tAVH violation on ADDR by %t", $realtime, tm_adv_n_pos + tAVH - $realtime);
            end
        end
        tm_addr = $realtime;
    end

    task addr_timing_check;
    input i;
    integer i;
    begin
        if (!ce_n && !adv_n_in && !oe_n && we_n && (by_n == {BY_BITS{1'b0}})) begin
            case (i)
                3: begin if ($realtime - tm_page_addr3 < tPC) $display ("%t ERROR:   tPC violation on ADDR bit %d by %t", $realtime, i, tm_page_addr3 + tPC - $realtime); tm_page_addr3 <= $realtime; end
                2: begin if ($realtime - tm_page_addr2 < tPC) $display ("%t ERROR:   tPC violation on ADDR bit %d by %t", $realtime, i, tm_page_addr2 + tPC - $realtime); tm_page_addr2 <= $realtime; end
                1: begin if ($realtime - tm_page_addr1 < tPC) $display ("%t ERROR:   tPC violation on ADDR bit %d by %t", $realtime, i, tm_page_addr1 + tPC - $realtime); tm_page_addr1 <= $realtime; end
                0: begin if ($realtime - tm_page_addr0 < tPC) $display ("%t ERROR:   tPC violation on ADDR bit %d by %t", $realtime, i, tm_page_addr0 + tPC - $realtime); tm_page_addr0 <= $realtime; end
            endcase
        end
    end
    endtask

    always @(addr[3]) addr_timing_check(3);
    always @(addr[2]) addr_timing_check(2);
    always @(addr[1]) addr_timing_check(1);
    always @(addr[0]) addr_timing_check(0);

    always @(dq_in) begin
        if (!sync_access) begin
            if ($realtime - tm_write_end < tDH)
                $display ("%t ERROR:  tDH violation on DQ by %t", $realtime, tm_write_end + tDH - $realtime);
        end
        tm_dq_tdw <= $realtime;
    end

    // check tOW
    always @(posedge tow_check) begin
        if (!oe_n) begin
            if (dq_in !== {DQ_BITS{1'bz}})
                $display ("%t ERROR:  tOW violation on DQ", $realtime);
        end
        tow_check <= 1'b0;
    end


    // check tCEM
    always @(posedge ce2cem) begin
        if (deep_power_down_exit !== 1'b1)
            $display ("%t ERROR:  tCEM violation on CE#", $realtime);
    end

endmodule

module cellram_pathdelay (
    clk,
    adv_n,
    zz_n,
    ce_n,
    oe_n,
    we_n,
    by_n,
    addr,
    clk2waite,
    adv2dqe,
    adv2dq,
    adv2wi,
    zz2pd,
    ce2dqe,
    ce2dq,
    ce2cem,
    ce2wi,
    ce2waite,
    ce2wait,
    ce2rst,
    oe2dqe,
    oe2dq,
    soe2dq,
    oe2waite,
    oe2wait,
    we2dqe,
    we2dq,
    we2waite,
    by2dqe,
    by2dq,
    addr2dq,
    addr2wi,
    saddr2dq
);
    `include "cellram_parameters.vh"

    input clk;
    input adv_n;
    input zz_n;
    input ce_n;
    input oe_n;
    input we_n;
    input [BY_BITS-1:0] by_n;
    input [ADDR_BITS-1:0] addr;
    output clk2waite;
    output adv2dqe;
    output adv2dq;
    output adv2wi;
    output zz2pd;
    output ce2dqe;
    output ce2dq;
    output ce2cem;
    output ce2wi;
    output ce2waite;
    output ce2wait;
    output ce2rst;
    output oe2dqe;
    output oe2dq;
    output soe2dq;
    output oe2waite;
    output oe2wait;
    output we2dqe;
    output we2waite;
    output we2dq;
    output by2dqe;
    output by2dq;
    output addr2dq;
    output addr2wi;
    output saddr2dq;

    reg adv2dqr, addr2dqr, addr2wir, saddr2dqr, clk2waiter;

    always @(negedge adv_n) begin
        if (!adv_n) begin
            adv2dqr <= 1'b0;
            adv2dqr <= #(1) 1'b1;
        end
    end

    always @(addr[ADDR_BITS-1:4]) begin
        addr2dqr <= 1'b0;
        addr2dqr <= #(1) 1'b1;
    end

    always @(addr) begin
        addr2wir <= 1'b0;
        addr2wir <= #(1) 1'b1;
    end

    always @(posedge clk) begin
        if (!adv_n) begin
            saddr2dqr <= 1'b0;
            saddr2dqr <= #(1) 1'b1;
        end
    end

    always @(posedge ce_n or posedge clk) begin
        clk2waiter <= !ce_n;
    end

    assign clk2waite = clk2waiter;
    assign adv2dqe   = adv_n;
    assign adv2dq    = adv2dqr;
    assign adv2wi    = adv2dqr;
    assign zz2pd     = !zz_n;
    assign ce2dqe    = !ce_n;
    assign ce2dq     = !ce_n;
    assign ce2cem    = !ce_n;
    assign ce2wi     = !ce_n;
    assign ce2waite  = !ce_n;
    assign ce2wait   = !ce_n;
    assign ce2rst    = ce_n;
    assign oe2dqe    = !oe_n;
    assign oe2dq     = !oe_n;
    assign soe2dq    = !oe_n;
    assign oe2waite  = !oe_n;
    assign oe2wait   = !oe_n;
    assign we2dqe    = we_n;
    assign we2dq     = we_n;
    assign we2waite  = we_n;
    // CR1.0 byte-wide transfers are allowed on both burst READ and WRITE operations.
    // CR1.5 byte-wide transfers are allowed on burst WRITE operations only.
    assign by2dqe    = !(&by_n);
    assign by2dq     = !(&by_n);
    assign addr2dq   = addr2dqr;
    assign addr2wi   = addr2wir;
    assign saddr2dq  = saddr2dqr;

    specify
        specparam PATHPULSE$ = 0; // pulse reject and error limit

        (clk   =>clk2waite) = (   tKHTL,  tHZ);
        (adv_n =>  adv2dqe) = (    tALZ, tAHZ);
        (negedge adv_n => (adv2dq +: adv_n)) = (tAADV,  tOH);
        (negedge adv_n => (adv2wi +: adv_n)) = (tWI,  0);
        (zz_n  =>    zz2pd) = (    tDPD,    0);
        (ce_n  =>   ce2dqe) = (     tLZ,  tHZ);
        (ce_n  =>    ce2dq) = (     tCO,    0);
        (ce_n  =>   ce2cem) = (    tCEM,    0);
        (ce_n  =>    ce2wi) = (     tWI,    0);
        (ce_n  => ce2waite) = (tCEW_MIN,  tHZ);
        (ce_n  =>  ce2wait) = (tCEW_MAX,    0);
        (ce_n  =>   ce2rst) = (      15,    0);
        (oe_n  =>   oe2dqe) = (    tOLZ, tOHZ);
        (oe_n  =>    oe2dq) = (     tOE,    0);
        (oe_n  =>   soe2dq) = (    tBOE,    0);
        (oe_n  => oe2waite) = (tOEW_MIN,  tHZ);
        (oe_n  =>  oe2wait) = (tOEW_MAX,    0);
        (we_n  =>   we2dqe) = (       0, tWHZ);
        (we_n  =>    we2dq) = (       0,    0);
        (we_n  => we2waite) = (       0,    0);
        (by_n  *>   by2dqe) = (    tBLZ, tBHZ);
        (by_n  *>    by2dq) = (     tBA,    0);
        (addr  *>  addr2dq) = (     tAA,  tOH);
        (addr  *>  addr2wi) = (     tWI,    0);
        (posedge clk   => (saddr2dq +: clk)) = (tABA,  tKOH);
    endspecify

endmodule
