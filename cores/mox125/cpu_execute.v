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
                    branch_flag_o, branch_target_o, sel_o,
                    // Inputs
                    rst_i, clk_i, irq_i, dmem_data_i, dmem_ack_i, regA_i,
                    regB_i, pipeline_control_bits_i, register1_write_index_i,
                    register2_write_index_i, operand_i, op_i, sp_i, fp_i, PC_i,
                    pcrel_offset_i
                    );

  // Instructions that touch data memory run as a short sequence of
  // 16-bit wishbone cycles.  These are the sequences we know about.
  parameter [2:0] SEQ_STORE = 3'b000,
                  SEQ_LOAD = 3'b001,
                  SEQ_JSR = 3'b010,
                  SEQ_RET = 3'b011;

  // --- Clock and Reset ------------------------------------------
  input           rst_i, clk_i;

  // --- Interrupt request, level sensitive -----------------------
  input           irq_i;

  // --- Data memory ----------------------------------------------
  output reg [31:0] dmem_address_o;
  input [15:0]      dmem_data_i;
  output reg [15:0] dmem_data_o;
  output reg        dmem_stb_o;
  output reg [1:0]  dmem_sel_o;
  output reg        dmem_cyc_o;
  output reg        dmem_we_o;
  input             dmem_ack_i;

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

  // Register writes owed once the current sequence completes.
  reg                     pending_wea;
  reg                     pending_web;
  reg                     load_byte;
  reg                     load_to_b;

  // Special registers, read with gsr and written with ssr.
  //   sr0  status: bit 0 enables interrupts, bits 8:4 are the
  //        condition codes
  //   sr1  exception handler address
  //   sr2  exception cause: 2 interrupt, 3 swi, 5 brk
  //   sr3  swi number
  //   sr4  status as it was when the last exception was taken
  //   sr5  address to resume at after the exception
  //   sr6, sr7  scratch
  // Taking an exception copies sr0 to sr4, disables interrupts and
  // jumps to sr1.  Writing sr0 sets the condition codes as well as
  // the interrupt enable.
  //
  // To return, a handler writes the status it wants to resume with
  // to sr4 and then leaves with ret or jmp.  Writing sr4 arms the
  // restore, and the next ret or jmp copies sr4 back into sr0 as it
  // branches, so interrupts stay off until the handler is really gone.
  reg                     sr_ie;
  reg [31:0]              sr_handler;
  reg [2:0]               sr_cause;
  reg [31:0]              sr_swi;
  reg [5:0]               sr_saved_status;
  reg [31:0]              sr_resume;
  reg [31:0]              sr_scratch6;
  reg [31:0]              sr_scratch7;
  reg                     sr_restore_armed;

  wire                    take_irq = irq_i & sr_ie;

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
      dmem_cyc_o <= 1;
      dmem_stb_o <= 1;
      dmem_we_o <= we;
      dmem_sel_o <= sel;
      dmem_address_o <= address;
      dmem_data_o <= data;
      next_address <= address + 2;
      register_wea_o <= 0;
      register_web_o <= 0;
    end
  endtask

  task exception_enter;
    input [2:0]  cause;
    input [31:0] resume;
    begin
      sr_saved_status <= {CC_result, sr_ie};
      sr_restore_armed <= 0;
      sr_ie <= 0;
      sr_cause <= cause;
      sr_resume <= resume;
      branch_target_o <= sr_handler;
      branch_flag_o <= 1;
      register_wea_o <= 0;
      register_web_o <= 0;
      pipeline_control_bits_o <= 6'b0;
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
      sr_restore_armed <= 0;
      register_wea_o <= 0;
      register_web_o <= 0;
      pending_wea <= 0;
      pending_web <= 0;
      dmem_we_o <= 0;
      dmem_sel_o <= 2'b0;
      sel_o <= 2'b0;
      dmem_stb_o <= 0;
      dmem_cyc_o <= 0;
    end else begin
      if (div_active)
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
          // Hold the bus cycle until the slave acks it.
          if (dmem_ack_i)
            begin
              step <= step + 2'd1;
              if (step == last_step)
                begin
                  busy_o <= 0;
                  dmem_cyc_o <= 0;
                  dmem_stb_o <= 0;
                  dmem_we_o <= 0;
                  register_wea_o <= pending_wea;
                  register_web_o <= pending_web;
                  pipeline_control_bits_o <= 6'b0;
                end
              case (seq)
                SEQ_STORE:
                  begin
                    dmem_address_o <= next_address;
                    dmem_data_o <= next_data;
                  end
                SEQ_LOAD:
                  begin
                    read_hi <= dmem_data_i;
                    dmem_address_o <= next_address;
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
                        dmem_address_o <= sp_i - 6;
                        dmem_data_o <= return_address[15:0];
                      end
                    2'd1:
                      begin
                        dmem_address_o <= sp_i - 12;
                        dmem_data_o <= fp_i[31:16];
                      end
                    2'd2:
                      begin
                        dmem_address_o <= sp_i - 10;
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
                SEQ_RET:
                  // Pop $fp, pop the return address, skip the static chain.
                  case (step)
                    2'd0:
                      begin
                        read_hi <= dmem_data_i;
                        dmem_address_o <= fp_i + 2;
                      end
                    2'd1:
                      begin
                        reg2_result_o <= {read_hi, read_lo};
                        dmem_address_o <= fp_i + 4;
                      end
                    2'd2:
                      begin
                        read_hi <= dmem_data_i;
                        dmem_address_o <= fp_i + 6;
                      end
                    2'd3:
                      begin
                        branch_target_o <= {read_hi, read_lo};
                        if (sr_restore_armed)
                          begin
                            {CC_result, sr_ie} <= sr_saved_status;
                            sr_restore_armed <= 0;
                          end
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
              exception_enter (3'd2, PC_i);
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
                  exception_enter (3'd5, PC_plus_2);
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
                  case (incdec_value[2:0])
                    3'd0: reg1_result_o <= {23'b0, CC_result, 3'b0, sr_ie};
                    3'd1: reg1_result_o <= sr_handler;
                    3'd2: reg1_result_o <= {29'b0, sr_cause};
                    3'd3: reg1_result_o <= sr_swi;
                    3'd4: reg1_result_o <= {23'b0, sr_saved_status[5:1], 3'b0, sr_saved_status[0]};
                    3'd5: reg1_result_o <= sr_resume;
                    3'd6: reg1_result_o <= sr_scratch6;
                    3'd7: reg1_result_o <= sr_scratch7;
                  endcase // case (incdec_value[2:0])
                end
              `OP_INC:
                begin
                  reg1_result_o <= regA_i + { 24'b0, incdec_value };
                end
              `OP_JMP:
                begin
                  branch_target_o <= regA_i;
                  if (sr_restore_armed)
                    begin
                      {CC_result, sr_ie} <= sr_saved_status;
                      sr_restore_armed <= 0;
                    end
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
                  case (incdec_value[2:0])
                    3'd0:
                      begin
                        sr_ie <= regA_i[0];
                        CC_result <= regA_i[8:4];
                      end
                    3'd1: sr_handler <= regA_i;
                    3'd2: sr_cause <= regA_i[2:0];
                    3'd3: sr_swi <= regA_i;
                    3'd4:
                      begin
                        sr_saved_status <= {regA_i[8:4], regA_i[0]};
                        sr_restore_armed <= 1;
                      end
                    3'd5: sr_resume <= regA_i;
                    3'd6: sr_scratch6 <= regA_i;
                    3'd7: sr_scratch7 <= regA_i;
                  endcase // case (incdec_value[2:0])
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
                  exception_enter (3'd3, PC_plus_6);
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
