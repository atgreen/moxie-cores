// cpu_execute.v - The moxie execute stage
//
// Copyright (c) 2010, 2011, 2012, 2105, 2017, 2020, 2023 Anthony Green.
// DO NOT ALTER OR REMOVE COPYRIGHT NOTICES.
//
// The above named program is free software; you can redistribute it
// and/or modify it under the terms of the GNU General Public License
// version 2 as published by the Free Software Foundation.
//
// The above named program is distributed in the hope that it will be
// useful, but WITHOUT ANY WARRANTY; without even the implied warranty
// of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this work; if not, write to the Free Software
// Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA
// 02110-1301, USA.

`include "defines.h"

module cpu_execute (/*AUTOARG*/
                    // Outputs
                    dmem_address_o, dmem_data_o, dmem_stb_o, dmem_sel_o, dmem_cyc_o,
                    dmem_we_o, register_wea_o, register_web_o, register1_write_index_o,
                    register2_write_index_o, pipeline_control_bits_o, memory_address_o,
                    reg1_result_o, reg2_result_o, mem_result_o, PC_o, busy_o,
                    branch_flag_o, branch_target_o, sel_o, mmu_o, um_o, asid_o,
                    dtlb_vaddr_o, dtlb_we_o, itlb_we_o, tlbhi_o, tlblo_o,
                    icache_flush_o,
                    // Inputs
                    rst_i, clk_i, irq_i, dmem_data_i, dmem_ack_i, dtlb_hit_i,
                    dtlb_ppn_i, dtlb_user_i, dtlb_writable_i, ifault_i,
                    ifault_prot_i, ifault_addr_i, regA_i,
                    regB_i, pipeline_control_bits_i, register1_write_index_i,
                    register2_write_index_i, operand_i, op_i, sp_i, fp_i, r0_i, PC_i,
                    pcrel_offset_i
                    );

  // Instructions that touch data memory run as a short sequence of
  // 16-bit wishbone cycles.  These are the sequences we know about.
  parameter [2:0] SEQ_STORE = 3'b000,
                  SEQ_LOAD = 3'b001,
                  SEQ_JSR = 3'b010,
                  SEQ_RET = 3'b011,
                  SEQ_CAS = 3'b100;

  // --- Clock and Reset ------------------------------------------
  input           rst_i, clk_i;

  // --- Interrupt request, level sensitive -----------------------
  input           irq_i;

  // --- Data memory ----------------------------------------------
  output [31:0]     dmem_address_o;
  input [15:0]      dmem_data_i;
  output reg [15:0] dmem_data_o;
  output            dmem_stb_o;
  output reg [1:0]  dmem_sel_o;
  output            dmem_cyc_o;
  output            dmem_we_o;
  input             dmem_ack_i;

  // --- Memory management ----------------------------------------
  // Translation enabled, user mode, and the address space id, for
  // the instruction side and the TLBs.
  output            mmu_o;
  output            um_o;
  output [7:0]      asid_o;

  // The data TLB is looked up with the address of each bus cycle.
  output [31:0]     dtlb_vaddr_o;
  input             dtlb_hit_i;
  input [19:0]      dtlb_ppn_i;
  input             dtlb_user_i;
  input             dtlb_writable_i;

  // Writing the TLB control register stores tlbhi/tlblo in a TLB.
  output reg        dtlb_we_o;
  output reg        itlb_we_o;
  output [31:0]     tlbhi_o;
  output [31:0]     tlblo_o;

  output reg        icache_flush_o;

  // Fetch could not translate the address it needs next.
  input             ifault_i;
  input             ifault_prot_i;
  input [31:0]      ifault_addr_i;

  // --- Pipeline interlocking ------------------------------------
  // High while a multi-cycle instruction owns the execute stage.
  // The rest of the pipeline must hold still until it drops.
  output reg        busy_o;

  // --- Inputs ---------------------------------------------------
  input [31:0]      regA_i;
  input [31:0]      regB_i;
  input [`PCB_WIDTH-1:0] pipeline_control_bits_i;
  input [3:0]            register1_write_index_i;
  input [3:0]            register2_write_index_i;
  input [31:0]           operand_i;
  input [6:0]            op_i;
  input [31:0]           sp_i;
  input [31:0]           fp_i;
  input [31:0]           r0_i;
  input [31:0]           PC_i /*verilator public*/;
  input [9:0]            pcrel_offset_i;

  // --- Outputs --------------------------------------------------
  output                 register_wea_o;
  output                 register_web_o;
  output [3:0]           register1_write_index_o;
  output [3:0]           register2_write_index_o;
  output [`PCB_WIDTH-1:0] pipeline_control_bits_o;
  output [31:0]           memory_address_o;
  output [31:0]           reg1_result_o;
  output [31:0]           reg2_result_o;
  output [31:0]           mem_result_o;
  output [31:0]           PC_o;
  output [1:0]            sel_o;

  reg [0:4]               CC_result;

  output                  branch_flag_o;
  output [31:0]           branch_target_o;

  reg                     branch_flag_o;
  reg [31:0]              branch_target_o;

  reg [3:0]               register1_write_index_o;
  reg [3:0]               register2_write_index_o;
  reg [`PCB_WIDTH-1:0]    pipeline_control_bits_o;
  reg [31:0]              memory_address_o;
  reg [31:0]              reg1_result_o;
  reg [31:0]              reg2_result_o;
  reg [31:0]              mem_result_o;

  reg [31:0]              PC_o;
  reg [1:0]               sel_o;
  wire [31:0]             PC_plus_2 = PC_i+2;
  wire [31:0]             PC_plus_6 = PC_i+6;

  reg [2:0]               seq;
  reg [1:0]               step;
  reg [1:0]               last_step;

  reg [31:0]              next_address;
  reg [15:0]              next_data;
  reg [15:0]              read_hi;
  reg [31:0]              return_address;

  // cas.l: the value expected in memory and the one to replace it with.
  reg [31:0]              cas_expected;
  reg [31:0]              cas_new;

  // Register writes owed once the current sequence completes.
  reg                     pending_wea;
  reg                     pending_web;
  reg                     load_byte;
  reg                     load_to_b;

  // Special registers, read with gsr and written with ssr.  They are
  // privileged, as are eret and wfi: using any of them in user mode
  // is an exception.
  //   sr0   status: bit 0 enables interrupts, bit 2 is user mode,
  //         bit 3 turns address translation on, bits 8:4 are the
  //         condition codes.  Writing it only changes the interrupt
  //         enable and the condition codes.
  //   sr1   exception handler address
  //   sr2   exception cause, see below
  //   sr3   swi number
  //   sr4   status as it was when the last exception was taken
  //   sr5   address to resume at after the exception
  //   sr6, sr7  scratch
  //   sr8   address that could not be translated
  //   sr9   tlbhi: virtual page number and address space id
  //   sr10  tlblo: physical page number and flags
  //   sr11  writing stores tlbhi/tlblo in the data TLB (bit 0 clear)
  //         or the instruction TLB (bit 0 set)
  //   sr12  current address space id
  //   sr13  TLB miss handler address, or zero to use sr1
  //   sr14  writing empties the instruction cache
  //
  // Causes: 2 interrupt, 3 swi, 5 brk, 6 instruction TLB miss,
  // 7 data TLB miss on a load, 8 data TLB miss on a store,
  // 9 instruction protection, 10 data protection on a load,
  // 11 data protection on a store, 12 privileged instruction.
  //
  // Taking an exception copies sr0 to sr4, then disables interrupts,
  // leaves user mode, turns translation off and jumps to the handler.
  // A TLB miss or protection fault also puts the address in sr8 and
  // its page number, with the current address space id, in sr9.
  //
  // eret undoes that: it copies sr4 back to sr0 and jumps to sr5, as
  // one step, so the mode only changes as the handler leaves.  A
  // handler that may be interrupted must save sr4 and sr5 first and
  // put them back before it returns.  eret is also the only way to
  // enter user mode or to turn translation on.
  //
  // wfi stops the core until an interrupt is requested, whether or not
  // interrupts are enabled.  Waiting with them disabled and enabling
  // them afterwards cannot miss a request, because it stays asserted.
  reg                     sr_ie;
  reg                     sr_um;
  reg                     sr_mmu;
  reg [31:0]              sr_handler;
  reg [3:0]               sr_cause;
  reg [31:0]              sr_swi;
  reg [7:0]               sr_saved_status;
  reg [31:0]              sr_resume;
  reg [31:0]              sr_scratch6;
  reg [31:0]              sr_scratch7;
  reg [31:0]              sr_fault_address;
  reg [31:0]              sr_tlbhi;
  reg [31:0]              sr_tlblo;
  reg [7:0]               sr_asid;
  reg [31:0]              sr_tlb_handler;
  reg                     waiting;

  wire                    take_irq = irq_i & sr_ie;

  assign mmu_o = sr_mmu;
  assign um_o = sr_um;
  assign asid_o = sr_asid;
  assign tlbhi_o = sr_tlbhi;
  assign tlblo_o = sr_tlblo;

  // The bus request as the sequencer sees it, before translation.
  reg [31:0]              dmem_vaddr;
  reg                     dmem_stb;
  reg                     dmem_cyc;
  reg                     dmem_we;

  wire                    dmem_miss = sr_mmu & !dtlb_hit_i;
  wire                    dmem_prot = sr_mmu & dtlb_hit_i
                          & ((sr_um & !dtlb_user_i) | (dmem_we & !dtlb_writable_i));
  wire                    dmem_fault = dmem_stb & (dmem_miss | dmem_prot);

  // A cycle that cannot be translated never reaches the bus.
  assign dtlb_vaddr_o = dmem_vaddr;
  assign dmem_address_o = sr_mmu ? {dtlb_ppn_i, dmem_vaddr[11:0]} : dmem_vaddr;
  assign dmem_stb_o = dmem_stb & !dmem_fault;
  assign dmem_cyc_o = dmem_cyc & !dmem_fault;
  assign dmem_we_o = dmem_we;

  // One shared restoring divider, a quotient bit per clock.  It works
  // on magnitudes and fixes up the sign of the result at the end.
  reg                     div_active;
  reg                     div_fixup;
  reg [4:0]               div_count;
  reg [31:0]              div_quotient;
  reg [31:0]              div_remainder;
  reg [31:0]              div_divisor;
  reg                     div_want_remainder;
  reg                     div_negate;

  wire [32:0]             div_trial = {div_remainder, div_quotient[31]};
  wire [32:0]             div_diff = div_trial - {1'b0, div_divisor};
  wire                    div_fits = !div_diff[32];
  wire [31:0]             div_next_remainder = div_fits ? div_diff[31:0] : div_trial[31:0];
  wire [31:0]             div_next_quotient = {div_quotient[30:0], div_fits};
  wire [31:0]             div_magnitude = div_want_remainder ? div_remainder : div_quotient;

  reg                     register_wea_o;
  reg                     register_web_o;

  wire                    cc_eq, cc_gt, cc_lt, cc_gtu, cc_ltu;
  wire [31:0]             cc_AsubB;
  wire [31:0]             cc_BsubA;

  assign cc_AsubB = regA_i - regB_i;
  assign cc_BsubA = regB_i - regA_i;

  assign cc_eq = regA_i == regB_i;
  assign cc_lt = regA_i[31] ^ regB_i[31] ? regA_i[31] : cc_AsubB[31];
  assign cc_gt = regA_i[31] ^ regB_i[31] ? regB_i[31] : cc_BsubA[31];
  assign cc_gtu = regA_i > regB_i;
  assign cc_ltu = regA_i < regB_i;

  wire                    branch_condition;
  assign branch_condition =
                           ((op_i == `OP_BEQ) & CC_result[0])
    | ((op_i == `OP_BNE) & !CC_result[0])
      | ((op_i == `OP_BLT) & CC_result[1])
        | ((op_i == `OP_BLTU) & CC_result[3])
          | ((op_i == `OP_BGT) & CC_result[2])
            | ((op_i == `OP_BGTU) & CC_result[4])
              | ((op_i == `OP_BLE) & (CC_result[0] | CC_result[1]))
                | ((op_i == `OP_BGE) & (CC_result[0] | CC_result[2]))
                  | ((op_i == `OP_BLEU) & (CC_result[0] | CC_result[3]))
                    | ((op_i == `OP_BGEU) & (CC_result[0] | CC_result[4]));

  wire [31:0]             pcrel_branch_target;
  assign pcrel_branch_target = {{21{pcrel_offset_i[9]}}, pcrel_offset_i, 1'b0 } + PC_i + 32'd2;

  wire [7:0]              incdec_value = pcrel_offset_i[7:0];

  wire [15:0]             read_lo = dmem_data_i;
  wire [31:0]             read_value = (last_step == 2'd0)
                          ? (load_byte ? {24'b0, dmem_data_i[15:8]} : {16'b0, dmem_data_i})
                          : {read_hi, read_lo};

  // Start a data memory sequence of LAST+1 bus cycles.  Any register
  // write is deferred until the sequence completes.
  task mem_start;
    input [2:0]  sq;
    input [1:0]  last;
    input        we;
    input [1:0]  sel;
    input [31:0] address;
    input [15:0] data;
    begin
      busy_o <= 1;
      seq <= sq;
      step <= 2'd0;
      last_step <= last;
      dmem_cyc <= 1;
      dmem_stb <= 1;
      dmem_we <= we;
      dmem_sel_o <= sel;
      dmem_vaddr <= address;
      dmem_data_o <= data;
      next_address <= address + 2;
      register_wea_o <= 0;
      register_web_o <= 0;
    end
  endtask

  task exception_enter;
    input [3:0]  cause;
    input [31:0] resume;
    begin
      sr_saved_status <= {CC_result, sr_mmu, sr_um, sr_ie};
      sr_ie <= 0;
      sr_um <= 0;
      sr_mmu <= 0;
      sr_cause <= cause;
      sr_resume <= resume;
      // TLB misses can have a handler of their own.
      if ((cause >= 4'd6) && (cause <= 4'd8) && (sr_tlb_handler != 32'b0))
        branch_target_o <= sr_tlb_handler;
      else
        branch_target_o <= sr_handler;
      branch_flag_o <= 1;
      register_wea_o <= 0;
      register_web_o <= 0;
      pipeline_control_bits_o <= 6'b0;
    end
  endtask

  // An address could not be translated.  Leave it, and the tlbhi
  // value an entry for it would need, where the handler can find them.
  task fault_enter;
    input [3:0]  cause;
    input [31:0] resume;
    input [31:0] address;
    begin
      exception_enter (cause, resume);
      sr_fault_address <= address;
      sr_tlbhi <= {address[31:12], 4'b0, sr_asid};
    end
  endtask

  // Start a divide.  The quotient takes the sign of the two operands
  // combined, the remainder takes the sign of the dividend.
  task div_start;
    input        is_signed;
    input        want_remainder;
    begin
      busy_o <= 1;
      div_active <= 1;
      div_fixup <= 0;
      div_count <= 5'd31;
      div_remainder <= 32'b0;
      div_quotient <= (is_signed & regA_i[31]) ? -regA_i : regA_i;
      div_divisor <= (is_signed & regB_i[31]) ? -regB_i : regB_i;
      div_want_remainder <= want_remainder;
      div_negate <= is_signed & (want_remainder ? regA_i[31] : (regA_i[31] ^ regB_i[31]));
      register_wea_o <= 0;
      register_web_o <= 0;
      pending_wea <= 1;
    end
  endtask

  always @(posedge rst_i or posedge clk_i)
    if (rst_i) begin
      pipeline_control_bits_o <= 6'b0;
      branch_flag_o <= 0;
      busy_o <= 0;
      div_active <= 0;
      sr_ie <= 0;
      sr_um <= 0;
      sr_mmu <= 0;
      sr_asid <= 8'b0;
      sr_tlb_handler <= 32'b0;
      waiting <= 0;
      dtlb_we_o <= 0;
      itlb_we_o <= 0;
      icache_flush_o <= 0;
      register_wea_o <= 0;
      register_web_o <= 0;
      pending_wea <= 0;
      pending_web <= 0;
      dmem_we <= 0;
      dmem_sel_o <= 2'b0;
      sel_o <= 2'b0;
      dmem_stb <= 0;
      dmem_cyc <= 0;
    end else begin
      dtlb_we_o <= 0;
      itlb_we_o <= 0;
      icache_flush_o <= 0;

      if (waiting)
        begin
          // Idle until something wants attention.
          if (irq_i)
            begin
              waiting <= 0;
              busy_o <= 0;
            end
        end
      else if (div_active)
        begin
          if (div_fixup)
            begin
              // The magnitude is ready; apply the sign and finish.
              busy_o <= 0;
              div_active <= 0;
              reg1_result_o <= div_negate ? -div_magnitude : div_magnitude;
              register_wea_o <= pending_wea;
            end
          else
            begin
              div_count <= div_count - 5'd1;
              div_remainder <= div_next_remainder;
              div_quotient <= div_next_quotient;
              div_fixup <= (div_count == 5'd0);
            end
        end
      else if (busy_o)
        begin
          if (dmem_fault)
            begin
              // Give the instruction up.  Nothing it does to a register
              // has happened yet, and what it has written to memory it
              // will simply write again when it is restarted.
              busy_o <= 0;
              dmem_cyc <= 0;
              dmem_stb <= 0;
              dmem_we <= 0;
              fault_enter (dmem_miss ? (dmem_we ? 4'd8 : 4'd7) : (dmem_we ? 4'd11 : 4'd10),
                           PC_o, dmem_vaddr);
            end
          // Hold the bus cycle until the slave acks it.
          else if (dmem_ack_i)
            begin
              step <= step + 2'd1;
              if (step == last_step)
                begin
                  busy_o <= 0;
                  dmem_cyc <= 0;
                  dmem_stb <= 0;
                  dmem_we <= 0;
                  register_wea_o <= pending_wea;
                  register_web_o <= pending_web;
                  pipeline_control_bits_o <= 6'b0;
                end
              case (seq)
                SEQ_STORE:
                  begin
                    dmem_vaddr <= next_address;
                    dmem_data_o <= next_data;
                  end
                SEQ_LOAD:
                  begin
                    read_hi <= dmem_data_i;
                    dmem_vaddr <= next_address;
                    if (step == last_step)
                      begin
                        if (load_to_b)
                          reg2_result_o <= read_value;
                        else
                          reg1_result_o <= read_value;
                      end
                  end
                SEQ_JSR:
                  // Return address at $sp-8, old $fp at $sp-12.  The
                  // slot at $sp-4 is reserved for the static chain.
                  case (step)
                    2'd0:
                      begin
                        dmem_vaddr <= sp_i - 6;
                        dmem_data_o <= return_address[15:0];
                      end
                    2'd1:
                      begin
                        dmem_vaddr <= sp_i - 12;
                        dmem_data_o <= fp_i[31:16];
                      end
                    2'd2:
                      begin
                        dmem_vaddr <= sp_i - 10;
                        dmem_data_o <= fp_i[15:0];
                      end
                    2'd3:
                      begin
                        // $sp and $fp both end up pointing at the saved $fp.
                        reg1_result_o <= sp_i - 12;
                        register1_write_index_o <= 4'd1;
                        reg2_result_o <= sp_i - 12;
                        register2_write_index_o <= 4'd0;
                        branch_flag_o <= 1;
                      end
                  endcase // case (step)
                SEQ_CAS:
                  // Read the word; if it is what $r0 says it should be,
                  // write the new value.  Nothing can come between the
                  // two, because interrupts are only taken between
                  // instructions.  $r0 gets what was read either way,
                  // and the "equal" condition says whether we wrote.
                  case (step)
                    2'd0:
                      begin
                        read_hi <= dmem_data_i;
                        dmem_vaddr <= next_address;
                      end
                    2'd1:
                      begin
                        reg1_result_o <= {read_hi, read_lo};
                        register1_write_index_o <= 4'd2;
                        if ({read_hi, read_lo} == cas_expected)
                          begin
                            CC_result <= 5'b10000;
                            dmem_vaddr <= next_address - 2;
                            dmem_data_o <= cas_new[31:16];
                            dmem_we <= 1;
                          end
                        else
                          begin
                            CC_result <= 5'b00000;
                            busy_o <= 0;
                            dmem_cyc <= 0;
                            dmem_stb <= 0;
                            register_wea_o <= 1;
                          end
                      end
                    2'd2:
                      begin
                        dmem_vaddr <= next_address;
                        dmem_data_o <= cas_new[15:0];
                      end
                    2'd3:
                      begin
                      end
                  endcase // case (step)
                SEQ_RET:
                  // Pop $fp, pop the return address, skip the static chain.
                  case (step)
                    2'd0:
                      begin
                        read_hi <= dmem_data_i;
                        dmem_vaddr <= fp_i + 2;
                      end
                    2'd1:
                      begin
                        reg2_result_o <= {read_hi, read_lo};
                        dmem_vaddr <= fp_i + 4;
                      end
                    2'd2:
                      begin
                        read_hi <= dmem_data_i;
                        dmem_vaddr <= fp_i + 6;
                      end
                    2'd3:
                      begin
                        branch_target_o <= {read_hi, read_lo};
                        reg1_result_o <= fp_i + 12;
                        register1_write_index_o <= 4'd1;
                        register2_write_index_o <= 4'd0;
                        branch_flag_o <= 1;
                      end
                  endcase // case (step)
                default:
                  begin
                  end
              endcase // case (seq)
            end
        end
      else if (branch_flag_o)
        begin
          /* We've just branched, so ignore the incoming instruction.  */
          branch_flag_o <= 0;
          pipeline_control_bits_o <= 6'b0;
          register_wea_o <= 0;
          register_web_o <= 0;
        end
      else
        begin
          branch_flag_o <= branch_condition | (op_i == `OP_JMPA) | (op_i == `OP_JMP);
          PC_o <= PC_i;
          pipeline_control_bits_o <= pipeline_control_bits_i;
          register_wea_o <= pipeline_control_bits_i[`PCB_WA];
          register_web_o <= pipeline_control_bits_i[`PCB_WB];
          register1_write_index_o <= register1_write_index_i;
          register2_write_index_o <= register2_write_index_i;
          pending_wea <= 0;
          pending_web <= 0;
          load_byte <= 0;
          load_to_b <= 0;
            if (take_irq)
              // Take the interrupt in place of the incoming instruction,
              // which runs when the handler resumes at its address.
              exception_enter (4'd2, PC_i);
            else if (ifault_i & (op_i == `OP_NOP))
              // Nothing is left ahead of the instruction fetch is stuck
              // on, so the fault is real and it is that instruction's.
              fault_enter (ifault_prot_i ? 4'd9 : 4'd6, PC_i, ifault_addr_i);
            else if (sr_um & ((op_i == `OP_GSR) | (op_i == `OP_SSR)
                              | (op_i == `OP_ERET) | (op_i == `OP_WFI)))
              exception_enter (4'd12, PC_i);
            else
            case (op_i)
              `OP_ADD_L:
                begin
                  reg1_result_o <= regA_i + regB_i;
                end
              `OP_AND:
                begin
                  reg1_result_o <= regA_i & regB_i;
                end
              `OP_ASHL:
                begin
                  reg1_result_o <= regA_i <<< regB_i;
                end
              `OP_ASHR:
                begin
                  reg1_result_o <= $signed(regA_i) >>> regB_i;
                end
              `OP_BAD:
                begin
                end
              `OP_BEQ:
                begin
                  branch_target_o <= pcrel_branch_target;
                end
              `OP_BGE:
                begin
                  branch_target_o <= pcrel_branch_target;
                end
              `OP_BGEU:
                begin
                  branch_target_o <= pcrel_branch_target;
                end
              `OP_BGT:
                begin
                  branch_target_o <= pcrel_branch_target;
                end
              `OP_BGTU:
                begin
                  branch_target_o <= pcrel_branch_target;
                end
              `OP_BLE:
                begin
                  branch_target_o <= pcrel_branch_target;
                end
              `OP_BLEU:
                begin
                  branch_target_o <= pcrel_branch_target;
                end
              `OP_BLT:
                begin
                  branch_target_o <= pcrel_branch_target;
                end
              `OP_BLTU:
                begin
                  branch_target_o <= pcrel_branch_target;
                end
              `OP_BNE:
                begin
                  branch_target_o <= pcrel_branch_target;
                end
              `OP_BRK:
                begin
                  exception_enter (4'd5, PC_plus_2);
                end
              `OP_CAS_L:
                begin
                  mem_start (SEQ_CAS, 2'd3, 1'b0, 2'b11, regA_i, 16'b0);
                  cas_expected <= r0_i;
                  cas_new <= regB_i;
                  pending_wea <= 1;
                end
              `OP_CMP:
                begin
                  CC_result <= {cc_eq, cc_lt, cc_gt, cc_ltu, cc_gtu};
                end
              `OP_DEC:
                begin
                  reg1_result_o <= regA_i - { 24'b0, incdec_value };
                end
              `OP_DIV_L:
                begin
                  div_start (1'b1, 1'b0);
                end
              `OP_GSR:
                begin
                  case (incdec_value[3:0])
                    4'd0: reg1_result_o <= {23'b0, CC_result, sr_mmu, sr_um, 1'b0, sr_ie};
                    4'd1: reg1_result_o <= sr_handler;
                    4'd2: reg1_result_o <= {28'b0, sr_cause};
                    4'd3: reg1_result_o <= sr_swi;
                    4'd4: reg1_result_o <= {23'b0, sr_saved_status[7:1], 1'b0, sr_saved_status[0]};
                    4'd5: reg1_result_o <= sr_resume;
                    4'd6: reg1_result_o <= sr_scratch6;
                    4'd7: reg1_result_o <= sr_scratch7;
                    4'd8: reg1_result_o <= sr_fault_address;
                    4'd9: reg1_result_o <= sr_tlbhi;
                    4'd10: reg1_result_o <= sr_tlblo;
                    4'd12: reg1_result_o <= {24'b0, sr_asid};
                    4'd13: reg1_result_o <= sr_tlb_handler;
                    default: reg1_result_o <= 32'b0;
                  endcase // case (incdec_value[3:0])
                end
              `OP_INC:
                begin
                  reg1_result_o <= regA_i + { 24'b0, incdec_value };
                end
              `OP_JMP:
                begin
                  branch_target_o <= regA_i;
                end
              `OP_ERET:
                begin
                  branch_target_o <= sr_resume;
                  branch_flag_o <= 1;
                  {CC_result, sr_mmu, sr_um, sr_ie} <= sr_saved_status;
                end
              `OP_WFI:
                begin
                  waiting <= 1;
                  busy_o <= 1;
                end
              `OP_JMPA:
                begin
                  branch_target_o <= operand_i;
                end
              `OP_JSR:
                begin
                  mem_start (SEQ_JSR, 2'd3, 1'b1, 2'b11, sp_i - 8, PC_plus_2[31:16]);
                  return_address <= PC_plus_2;
                  branch_target_o <= regA_i;
                  pending_wea <= 1;
                  pending_web <= 1;
                end
              `OP_JSRA:
                begin
                  mem_start (SEQ_JSR, 2'd3, 1'b1, 2'b11, sp_i - 8, PC_plus_6[31:16]);
                  return_address <= PC_plus_6;
                  branch_target_o <= operand_i;
                  pending_wea <= 1;
                  pending_web <= 1;
                end
              `OP_LDA_B:
                begin
                  mem_start (SEQ_LOAD, 2'd0, 1'b0, 2'b10, operand_i, 16'b0);
                  pending_wea <= 1;
                  load_byte <= 1;
                end
              `OP_LDA_L:
                begin
                  mem_start (SEQ_LOAD, 2'd1, 1'b0, 2'b11, operand_i, 16'b0);
                  pending_wea <= 1;
                end
              `OP_LDA_S:
                begin
                  mem_start (SEQ_LOAD, 2'd0, 1'b0, 2'b11, operand_i, 16'b0);
                  pending_wea <= 1;
                end
              `OP_LD_B:
                begin
                  mem_start (SEQ_LOAD, 2'd0, 1'b0, 2'b10, regB_i, 16'b0);
                  pending_wea <= 1;
                  load_byte <= 1;
                end
              `OP_LDI_B:
                begin
                  reg1_result_o <= operand_i;
                end
              `OP_LDI_L:
                begin
                  reg1_result_o <= operand_i;
                end
              `OP_LDI_S:
                begin
                  reg1_result_o <= operand_i;
                end
              `OP_LD_L:
                begin
                  mem_start (SEQ_LOAD, 2'd1, 1'b0, 2'b11, regB_i, 16'b0);
                  pending_wea <= 1;
                end
              `OP_LDO_B:
                begin
                  mem_start (SEQ_LOAD, 2'd0, 1'b0, 2'b10, operand_i + regB_i, 16'b0);
                  pending_wea <= 1;
                  load_byte <= 1;
                end
              `OP_LDO_L:
                begin
                  mem_start (SEQ_LOAD, 2'd1, 1'b0, 2'b11, operand_i + regB_i, 16'b0);
                  pending_wea <= 1;
                end
              `OP_LDO_S:
                begin
                  mem_start (SEQ_LOAD, 2'd0, 1'b0, 2'b11, operand_i + regB_i, 16'b0);
                  pending_wea <= 1;
                end
              `OP_LD_S:
                begin
                  mem_start (SEQ_LOAD, 2'd0, 1'b0, 2'b11, regB_i, 16'b0);
                  pending_wea <= 1;
                end
              `OP_LSHR:
                begin
                  reg1_result_o <= regA_i >> regB_i;
                end
              `OP_MOD_L:
                begin
                  div_start (1'b1, 1'b1);
                end
              `OP_MOV:
                begin
                  reg1_result_o <= regB_i;
                end
              `OP_MUL_L:
                begin
                  reg1_result_o <= regA_i * regB_i;
                end
              `OP_NEG:
                begin
                  reg1_result_o <= -regB_i;
                end
              `OP_NOP:
                begin
                end
              `OP_NOT:
                begin
                  reg1_result_o <= ~regB_i;
                end
              `OP_OR:
                begin
                  reg1_result_o <= regA_i | regB_i;
                end
              `OP_POP:
                begin
                  // Load the value into register B, then bump the pointer
                  // in register A by 4 bytes.
                  mem_start (SEQ_LOAD, 2'd1, 1'b0, 2'b11, regA_i, 16'b0);
                  reg1_result_o <= regA_i + 4;
                  pending_wea <= 1;
                  pending_web <= 1;
                  load_to_b <= 1;
                end
              `OP_PUSH:
                begin
                  // Decrement pointer register by 4 bytes.
                  mem_start (SEQ_STORE, 2'd1, 1'b1, 2'b11, regA_i - 4, regB_i[31:16]);
                  next_data <= regB_i[15:0];
                  reg1_result_o <= regA_i - 4;
                  pending_wea <= 1;
                  sel_o <= 2'b11;
                end
              `OP_RET:
                begin
                  mem_start (SEQ_RET, 2'd3, 1'b0, 2'b11, fp_i, 16'b0);
                  pending_wea <= 1;
                  pending_web <= 1;
                end
              `OP_SEX_B:
                begin
                  reg1_result_o <= { {24{regB_i[7]}}, regB_i[7:0] };
                end
              `OP_SEX_S:
                begin
                  reg1_result_o <= { {16{regB_i[15]}}, regB_i[15:0] };
                end
              `OP_ZEX_B:
                begin
                  reg1_result_o <= { 24'b0, regB_i[7:0] };
                end
              `OP_ZEX_S:
                begin
                  reg1_result_o <= { 16'b0, regB_i[15:0] };
                end
              `OP_SSR:
                begin
                  case (incdec_value[3:0])
                    4'd0:
                      begin
                        sr_ie <= regA_i[0];
                        CC_result <= regA_i[8:4];
                      end
                    4'd1: sr_handler <= regA_i;
                    4'd2: sr_cause <= regA_i[3:0];
                    4'd3: sr_swi <= regA_i;
                    4'd4: sr_saved_status <= {regA_i[8:2], regA_i[0]};
                    4'd5: sr_resume <= regA_i;
                    4'd6: sr_scratch6 <= regA_i;
                    4'd7: sr_scratch7 <= regA_i;
                    4'd8: sr_fault_address <= regA_i;
                    4'd9: sr_tlbhi <= regA_i;
                    4'd10: sr_tlblo <= regA_i;
                    4'd11:
                      begin
                        dtlb_we_o <= !regA_i[0];
                        itlb_we_o <= regA_i[0];
                      end
                    4'd12: sr_asid <= regA_i[7:0];
                    4'd13: sr_tlb_handler <= regA_i;
                    4'd14: icache_flush_o <= 1;
                    default:
                      begin
                      end
                  endcase // case (incdec_value[3:0])
                end
              `OP_STA_B:
                begin
                  mem_start (SEQ_STORE, 2'd0, 1'b1, 2'b01, operand_i, {8'b0, regA_i[7:0]});
                  mem_result_o <= {regA_i[7:0], regA_i[7:0], regA_i[7:0], regA_i[7:0]};
                  memory_address_o <= operand_i;
                  sel_o <= 2'b01;
                end
              `OP_STA_L:
                begin
                  mem_start (SEQ_STORE, 2'd1, 1'b1, 2'b11, operand_i, regA_i[31:16]);
                  next_data <= regA_i[15:0];
                  mem_result_o <= regA_i;
                  memory_address_o <= operand_i;
                  sel_o <= 2'b11;
                end
              `OP_STA_S:
                begin
                  mem_start (SEQ_STORE, 2'd0, 1'b1, 2'b11, operand_i, regA_i[15:0]);
                  mem_result_o <= {regA_i[15:0], regA_i[15:0]};
                  memory_address_o <= operand_i;
                  sel_o <= 2'b11;
                end
              `OP_ST_B:
                begin
                  mem_start (SEQ_STORE, 2'd0, 1'b1, 2'b01, regA_i, {8'b0, regB_i[7:0]});
                  mem_result_o <= {regB_i[7:0], regB_i[7:0], regB_i[7:0], regB_i[7:0]};
                  memory_address_o <= regA_i;
                  sel_o <= 2'b01;
                end
              `OP_ST_L:
                begin
                  mem_start (SEQ_STORE, 2'd1, 1'b1, 2'b11, regA_i, regB_i[31:16]);
                  next_data <= regB_i[15:0];
                  mem_result_o <= regB_i;
                  memory_address_o <= regA_i;
                  sel_o <= 2'b11;
                end
              `OP_STO_B:
                begin
                  mem_start (SEQ_STORE, 2'd0, 1'b1, 2'b01, operand_i + regA_i, {8'b0, regB_i[7:0]});
                  mem_result_o <= {regB_i[7:0], regB_i[7:0], regB_i[7:0], regB_i[7:0]};
                  memory_address_o <= operand_i + regA_i;
                  sel_o <= 2'b01;
                end
              `OP_STO_L:
                begin
                  mem_start (SEQ_STORE, 2'd1, 1'b1, 2'b11, operand_i + regA_i, regB_i[31:16]);
                  next_data <= regB_i[15:0];
                  mem_result_o <= regB_i;
                  memory_address_o <= operand_i + regA_i;
                  sel_o <= 2'b11;
                end
              `OP_STO_S:
                begin
                  mem_start (SEQ_STORE, 2'd0, 1'b1, 2'b11, operand_i + regA_i, regB_i[15:0]);
                  mem_result_o <= {regB_i[15:0], regB_i[15:0]};
                  memory_address_o <= operand_i + regA_i;
                  sel_o <= 2'b11;
                end
              `OP_ST_S:
                begin
                  mem_start (SEQ_STORE, 2'd0, 1'b1, 2'b11, regA_i, regB_i[15:0]);
                  mem_result_o <= {regB_i[15:0], regB_i[15:0]};
                  memory_address_o <= regA_i;
                  sel_o <= 2'b11;
                end
              `OP_SUB_L:
                begin
                  reg1_result_o <= regA_i - regB_i;
                end
              `OP_SWI:
                begin
                  sr_swi <= operand_i;
                  exception_enter (4'd3, PC_plus_6);
                end
              `OP_UDIV_L:
                begin
                  div_start (1'b0, 1'b0);
                end
              `OP_UMOD_L:
                begin
                  div_start (1'b0, 1'b1);
                end
              `OP_XOR:
                begin
                  reg1_result_o <= regA_i ^ regB_i;
                end
            endcase // case (op_i)
          end
    end // else: !if(rst_i)
endmodule // cpu_execute;
