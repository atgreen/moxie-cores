// moxibit.v - A bit-serial moxie core
//
// Copyright (c) 2026 Anthony Green.  All Rights Reserved.
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

// moxibit computes one bit per clock, in the style of SERV.  Nothing is
// pipelined.  Each instruction is a short microprogram, written in
// microcode.org: a sequence of 16-bit bus cycles and passes, a pass
// being 32 clocks that stream every operand through the datapath LSB
// first.  The microcode and dispatch tables are ROMs, so on an FPGA
// they go in block RAM along with the register file.
//
// There are two 32-bit shift registers.  ADR drives the bus address
// and holds the PC between instructions.  DBUF takes bus data into its
// low half and drives bus data from its high half; a 16-clock rotate
// in between moves one half to the other.  The register file holds
// the moxie registers, the special registers other than the status
// word, and a few slots the microcode uses as scratch.
//
// The bus is mox125's.  The address is the byte address.  A short, and
// each half of a long, is the byte at the address on the high lane and
// the byte after it on the low lane.  A byte is read (sel 10) on the
// high lane and written (sel 01) from the low lane, wherever it is in
// memory.  There is always a clock with cyc low between two requests,
// so a slave that registers its ack cannot ack the next one early.
//
// With MDU = 0, mul, div, udiv, mod and umod are undefined
// instructions, for software to emulate.  With COMPACT = 0, so are the
// compact-code extension's (docs/compact.md), for about 15% fewer LUTs.
// FAST_INC = 1 spends about 20 LUTs on adding 2 to ADR in parallel,
// for about an eighth fewer clocks.

module moxibit #(parameter BOOT_ADDRESS = 32'h00001000,
		parameter MDU = 1,
		parameter COMPACT = 1,
		parameter FAST_INC = 0) (
  input             clk_i,
  input             rst_i,

  output            wb_cyc_o,
  output            wb_stb_o,
  output            wb_we_o,
  output [31:0]     wb_adr_o,
  output [15:0]     wb_dat_o,
  output [1:0]      wb_sel_o,
  input [15:0]      wb_dat_i,
  input             wb_ack_i,

  input             irq_i
  );

`include "moxibit_ucode.vh"

  // S_START is the first clock of every µop: a bus µop stays in it
  // until its ack, and a pass primes the register file there and in
  // S_PREP.
  localparam S_START = 2'd0, S_PASS = 2'd1, S_DISP = 2'd2, S_PREP = 2'd3;

  reg [1:0]  state;
  reg [7:0]  upc;
  reg [4:0]  cnt;
  reg [15:0] ir;
  reg [31:0] adr;
  reg [31:0] dbuf;
  reg        c_pc, c_alu, c_mdu;
  reg        zacc;		// some bit seen this pass was set
  reg        sx;		// the sign of a byte or short
  reg        flag;		// shift amount >= 32; divider fits
  reg        dly_a, dly_r;	// ADR's and the remainder's last bit
  reg        bi;		// a multiplier step's bit; a shift's sign
  reg [4:0]  sh;		// the shift amount, negated for ashl
  reg        seen;		// a bit of the shift amount was set
  reg        sa, sb;		// signs of the dividend and divisor
  reg [4:0]  it;		// divider steps
  reg        fast;		// this pass is the 16-clock rotate of an inc
  reg        ie;
  reg        cc_eq, cc_lt, cc_gt, cc_ltu, cc_gtu;

  // --- Microcode -----------------------------------------------------

  wire [UWIDTH-1:0] uw;
  wire [7:0] disp;
  wire [7:0] rom_addr;

  wire [U_KIND_W-1:0] u_kind = uw[U_KIND_LSB +: U_KIND_W];
  wire [U_A_W-1:0]    u_a    = uw[U_A_LSB +: U_A_W];
  wire [U_B_W-1:0]    u_b    = uw[U_B_LSB +: U_B_W];
  wire [U_W_W-1:0]    u_w    = uw[U_W_LSB +: U_W_W];
  wire [U_WD_W-1:0]   u_wd   = uw[U_WD_LSB +: U_WD_W];
  wire [U_MASK_W-1:0] u_mask = uw[U_MASK_LSB +: U_MASK_W];
  wire [U_ADR_W-1:0]  u_adr  = uw[U_ADR_LSB +: U_ADR_W];
  wire [U_DBUF_W-1:0] u_dbuf = uw[U_DBUF_LSB +: U_DBUF_W];
  wire [U_X_W-1:0]    u_x    = uw[U_X_LSB +: U_X_W];
  wire [U_Y_W-1:0]    u_y    = uw[U_Y_LSB +: U_Y_W];
  wire [U_OP_W-1:0]   u_op   = uw[U_OP_LSB +: U_OP_W];
  wire [U_PC_W-1:0]   u_pc   = uw[U_PC_LSB +: U_PC_W];
  wire [U_K_W-1:0]    u_k    = uw[U_K_LSB +: U_K_W];
  wire [U_ROFF_W-1:0] u_roff = uw[U_ROFF_LSB +: U_ROFF_W];
  wire [U_SP_W-1:0]   u_sp   = uw[U_SP_LSB +: U_SP_W];
  wire [U_COND_W-1:0] u_cond = uw[U_COND_LSB +: U_COND_W];
  wire [U_NEXT_W-1:0] u_next = uw[U_NEXT_LSB +: U_NEXT_W];

  wire fetching = state == S_START & u_kind == KIND_FETCH;

  moxibit_ucode #(.MDU (MDU), .COMPACT (COMPACT)) ucode (.clk_i (clk_i),
				     .uaddr_i (rom_addr),
				     .uword_o (uw),
				     .daddr_i (fetching ? {1'b0, wb_dat_i[15:8]}
						: {1'b1, ir[15:8]}),
				     .disp_o (disp));

  wire sp_cmp     = u_sp == SP_CMP;
  wire sp_caseq   = u_sp == SP_CASEQ;
  wire sp_gsr     = u_sp == SP_GSR;
  wire sp_ssr     = u_sp == SP_SSR;
  wire sp_exc     = u_sp == SP_EXC;
  wire sp_eret    = u_sp == SP_ERET;
  wire sp_shamti  = COMPACT != 0 && u_sp == SP_SHAMTI;
  wire sp_bnext   = COMPACT != 0 && u_sp == SP_BNEXT;
  wire sp_bprev   = COMPACT != 0 && u_sp == SP_BPREV;
  wire sp_shamt   = u_sp == SP_SHAMT | sp_shamti;
  wire sp_shift   = u_sp == SP_SHIFT;
  wire sp_mul     = MDU != 0 && u_sp == SP_MUL;
  wire sp_dsign   = MDU != 0 && u_sp == SP_DSIGN;
  wire sp_dabs    = MDU != 0 && u_sp == SP_DABS;
  wire sp_diviter = MDU != 0 && u_sp == SP_DIVITER;
  wire sp_divq    = MDU != 0 && u_sp == SP_DIVQ;
  wire sp_divr    = MDU != 0 && u_sp == SP_DIVR;

  // gsr and ssr name a special register in the low bits of the
  // instruction.  sr0 is the status word; sr1-sr7 are in the register
  // file; the MMU's registers, sr8 and up, are not implemented.
  wire [7:0] srn = ir[7:0];
  wire sr_status = srn == 0;
  wire sr_in_rf = srn[7:3] == 0 & srn[2:0] != 0;

  // --- Register file -------------------------------------------------

  // Codes 16-31 are register file slots; the rest name the fields
  // of the instruction, a special register, $fp, $sp or $r0.
  // The short ldo.l and sto.l, opcodes 0x48-0x4f, and add3, 0x5a-0x5b,
  // have 3-bit A and B fields, bits 6:4 and 2:0, that name $r0-$r7;
  // add3's first source is the C field, bits 8, 7 and 3.
  wire add3 = COMPACT != 0 && ir[15:9] == 7'b0101101;
  wire short3 = COMPACT != 0 && (ir[15:11] == 5'b01001 | add3);

  // The A, B and C fields as register numbers, decoded once for the
  // three ports.
  wire [3:0] field_a = short3 ? {1'b0, ir[6:4]} + 4'd2 : ir[7:4];
  wire [3:0] field_b = short3 ? {1'b0, ir[2:0]} + 4'd2 : ir[3:0];
  wire [3:0] field_c = add3 ? {1'b0, ir[8:7], ir[3]} + 4'd2 : ir[11:8];

  function [4:0] regidx (input [4:0] code);
    if (code[4])
      regidx = code;
    else
      case (code[2:0])
	3'd1:    regidx = {1'b0, field_a};
	3'd2:    regidx = {1'b0, field_b};
	3'd3:    regidx = {1'b0, field_c};
	3'd4:    regidx = {2'b11, ir[2:0]};
	3'd6:    regidx = 5'd1;
	3'd7:    regidx = 5'd2;
	default: regidx = 5'd0;
      endcase
  endfunction

  wire ra, rb;
  wire rf_we = state == S_PASS & u_w != A_NONE & (!sp_ssr | sr_in_rf);
  wire rf_wd;

  moxibit_rf rf (.clk_i (clk_i),
		.prep_i (state != S_PASS),
		.first_i (state == S_START),
		.cnt_i (cnt),
		.reg_a_i (regidx (u_a)),
		.reg_b_i (regidx (u_b)),
		.half_b_i (u_roff == ROFF_HALF),
		.ra_o (ra),
		.rb_o (rb),
		.we_i (rf_we),
		.reg_w_i (regidx (u_w)),
		.wd_i (rf_wd));

  // --- The serial datapath -------------------------------------------

  wire aq = adr[0];
  wire dq = dbuf[0];
  wire kbit = cnt < 4 ? u_k[{1'b0, cnt[1:0]}] : u_k[4];

  // The status word: interrupt enable in bit 0, the condition codes in
  // bits 8:4.
  reg stat_bit;
  always @(*)
    case (cnt)
      5'd0: stat_bit = ie;
      5'd4: stat_bit = cc_gtu;
      5'd5: stat_bit = cc_ltu;
      5'd6: stat_bit = cc_gt;
      5'd7: stat_bit = cc_lt;
      5'd8: stat_bit = cc_eq;
      default: stat_bit = 1'b0;
    endcase

  reg take;
  always @(*)
    case (ir[13:10])
      4'd0: take = cc_eq;		// beq
      4'd1: take = !cc_eq;		// bne
      4'd2: take = cc_lt;		// blt
      4'd3: take = cc_gt;		// bgt
      4'd4: take = cc_ltu;		// bltu
      4'd5: take = cc_gtu;		// bgtu
      4'd6: take = cc_eq | cc_gt;	// bge
      4'd7: take = cc_eq | cc_lt;	// ble
      4'd8: take = cc_eq | cc_gtu;	// bgeu
      4'd9: take = cc_eq | cc_ltu;	// bleu
      default: take = 1'b1;		// bra
    endcase

  // The PC adder adds 2 by injecting a carry into bit 1, plus twice
  // the branch offset when a branch is taken.
  // beqz and bnez, conditions 11 and 14, have a 6-bit offset, bits 9:8
  // and 3:0.
  wire bz = COMPACT != 0 && (ir[13:10] == 4'd11 | ir[13:10] == 4'd14);
  wire [31:0] off = bz ? {{25{ir[9]}}, ir[9:8], ir[3:0], 1'b0}
		    : {{21{ir[9]}}, ir[9:0], 1'b0};
  wire pc_y = u_pc == PC_BR ? take & off[cnt] : u_pc == PC_K & kbit;
  wire pc_cin = c_pc | (u_pc == PC_P2 | u_pc == PC_BR) & cnt == 5'd1;
  wire pc_sum = aq ^ pc_y ^ pc_cin;
  wire pc_cout = aq & pc_y | aq & pc_cin | pc_y & pc_cin;

  // The multiplier and divider.  A divider step works on the
  // remainder R from the last step, which is still to have the
  // divisor taken off if the last step found that it fitted:
  // the MDU adder makes R' = R - fits * d, and the ALU tries
  // (R' << 1 | the next dividend bit) - d.
  wire neg_q = sa ^ sb;
  wire m_x = !sp_dabs & ra;
  wire m_y = sp_dabs ? rb ^ sb : !(dq & flag);
  wire m_cin = cnt == 5'd0 ? (sp_dabs ? sb : 1'b1) : c_mdu;
  wire mdu_sum = m_x ^ m_y ^ m_cin;
  wire mdu_cout = m_x & m_y | m_x & m_cin | m_y & m_cin;
  wire rs = cnt == 5'd0 ? adr[31] : dly_r;

  // ashli, lshri, ashri and cmpi have a 5-bit immediate, bit 8 and
  // bits 3:0; imm5 is its bit cnt, sign-extended.  A shift's count
  // comes from it (sp shamti) or from port B.
  wire imm5 = cnt < 5'd4 ? ir[{2'b0, cnt[1:0]}] : ir[8];

  // imm_bit is bit cnt of the immediate that y = i8 reads: the 8-bit
  // value of inc and dec; the 7-bit value of the short ldi.l, bit 10,
  // bits 9:8 and bits 3:0, sign-extended; the byte offset of the short
  // ldo.l and sto.l, four times bits 9, 8, 7 and 3; the -4(k + 1) of
  // their $fp forms, where k is imm5 unsigned; or cmpi's imm5.
  wire [1:0] cnt_m2 = cnt[1:0] - 2'd2;
  reg imm_bit;
  always @(*)
    if (ir[15:14] == 2'b10)
      imm_bit = cnt[4:3] == 0 & ir[{1'b0, cnt[2:0]}];
    else if (ir[15])
      imm_bit = cnt < 5'd4 ? ir[{2'b0, cnt[1:0]}]
		: cnt == 5'd4 ? ir[8] : cnt == 5'd5 ? ir[9] : ir[10];
    else if (short3)
      case (cnt)
	5'd2: imm_bit = ir[3];
	5'd3: imm_bit = ir[7];
	5'd4: imm_bit = ir[8];
	5'd5: imm_bit = ir[9];
	default: imm_bit = 1'b0;
      endcase
    else if (ir[15:10] == 6'b010100)
      imm_bit = cnt < 5'd2 ? 1'b0 : cnt < 5'd6 ? !ir[{2'b0, cnt_m2}]
		: cnt == 5'd6 ? !ir[8] : 1'b1;
    else
      imm_bit = imm5;

  // The ALU.
  reg y_raw;
  always @(*)
    case (u_y)
      Y_B:   y_raw = rb;
      Y_A:   y_raw = ra;
      Y_I8:  y_raw = COMPACT != 0 ? imm_bit
			  : cnt[4:3] == 0 & ir[{1'b0, cnt[2:0]}];
      Y_DQ:  y_raw = dq;
      Y_O16: y_raw = cnt[4] ? sx : dq;
      Y_K:   y_raw = kbit;
      Y_MB:  y_raw = aq & (cnt == 5'd0 ? dq : bi);
      default: y_raw = 1'b0;
    endcase

  reg x, y_in, yinv, c0;
  always @(*)
    begin
      x = u_x == X_A & ra;
      y_in = y_raw;
      yinv = u_op == OP_SUB | u_op == OP_NOT;
      c0 = u_op == OP_SUB;
      if (sp_dabs)
	begin yinv = sa; c0 = sa; end
      if (sp_diviter)
	begin x = rs; y_in = dq; yinv = 1'b1; c0 = 1'b1; end
      if (sp_divq)
	begin y_in = aq; yinv = neg_q; c0 = neg_q; end
      if (sp_divr)
	begin y_in = mdu_sum; yinv = sa; c0 = sa; end
    end

  wire y = y_in ^ yinv;
  wire alu_cin = cnt == 5'd0 ? c0 : c_alu;
  wire alu_sum = x ^ y ^ alu_cin;
  wire alu_cout = x & y | x & alu_cin | y & alu_cin;

  reg alu_out;
  always @(*)
    case (u_op)
      OP_AND: alu_out = x & y;
      OP_OR:  alu_out = x | y;
      OP_XOR: alu_out = x ^ y;
      OP_MOV, OP_NOT: alu_out = y;
      default: alu_out = alu_sum;
    endcase

  // What is shifted into each register.
  reg adr_in;
  always @(*)
    case (u_adr)
      ADR_PC:  adr_in = pc_sum;
      ADR_ALU: adr_in = alu_out;
      ADR_A:   adr_in = ra;
      ADR_B:   adr_in = rb;
      ADR_DQ:  adr_in = dq;
      ADR_ROT: adr_in = aq;
      ADR_SHL: adr_in = cnt != 5'd0 & dly_a;
      default: adr_in = aq;
    endcase

  reg dbuf_in;
  always @(*)
    case (u_dbuf)
      DBUF_A:    dbuf_in = ra;
      DBUF_B:    dbuf_in = rb;
      DBUF_PC:   dbuf_in = pc_sum;
      DBUF_SHR1: dbuf_in = cnt != 5'd31 & dbuf[1];
      DBUF_MDU:  dbuf_in = mdu_sum;
      default:   dbuf_in = dq;
    endcase
  wire dbuf_en = u_dbuf != DBUF_NONE & (u_dbuf != DBUF_ROT16 | !cnt[4]);

  // A shift streams its operand round DBUF, which in clock k holds
  // bits k to k+31 of it, rotated; the result is the bit SH along.
  // The first pass loaded SH, negated for ashl, so that this works
  // both ways: what remains is to fill the bits that come from beyond
  // either end.
  wire lsl = u_roff == ROFF_LSL;
  wire sh_past = {1'b0, cnt} + {1'b0, sh} > 6'd31;
  wire sh_fill = flag | (lsl ? sh != 0 & !sh_past : sh_past);
  wire shift_bit = sh_fill ? u_roff == ROFF_ASR & bi : dbuf[sh];
  wire sh_seen = cnt != 5'd0 & seen;
  wire sh_bit = sp_shamti ? cnt < 5 & imm5 : rb;

  // What is written to the register file.
  reg wd_raw;
  always @(*)
    if (sp_shift)
      wd_raw = shift_bit;
    else if (sp_gsr)
      wd_raw = sr_status ? stat_bit : sr_in_rf & ra;
    else if (sp_diviter)
      wd_raw = rs;
    else
      case (u_wd)
	WD_ALU:  wd_raw = alu_out;
	WD_PC:   wd_raw = pc_sum;
	WD_DQ:   wd_raw = dq;
	WD_D8:   wd_raw = dbuf[8];
	WD_A:    wd_raw = ra;
	WD_B:    wd_raw = rb;
	WD_STAT: wd_raw = stat_bit;
	default: wd_raw = 1'b0;
      endcase

  // ssr keeps only the bits of sr2 (the cause) and sr4 (the saved
  // status) that mox125 has.
  wire ssr_keep = srn == 2 ? cnt < 4
		: srn == 4 ? cnt == 0 | cnt >= 2 & cnt <= 8
		: 1'b1;
  wire mask_byte = u_mask == MASK_ZB | u_mask == MASK_SB;
  wire mask_sign = u_mask == MASK_SB | u_mask == MASK_SS;
  wire beyond = mask_byte ? cnt >= 8 : cnt[4];

  assign rf_wd = u_mask != MASK_NONE & beyond ? mask_sign & sx
	       : wd_raw & (!sp_ssr | ssr_keep);

  // Where the status word comes from: the register being written by
  // ssr 0, or sr4 for eret.
  wire set_status = sp_eret | sp_ssr & sr_status;

  // --- Sequencing ----------------------------------------------------

  wire bus_kind = u_kind == KIND_READ | u_kind == KIND_READB
		| u_kind == KIND_WRITE | u_kind == KIND_WRITEB
		| u_kind == KIND_FETCH;
  wire bus = state == S_START & bus_kind;
  wire zbit = sp_mul ? dbuf_in : alu_out;
  wire fits = mdu_sum | alu_cout;

  reg cond_true;
  always @(*)
    case (u_cond)
      COND_ZERO: cond_true = !(zacc | zbit);
      COND_IRQ:  cond_true = irq_i;
      COND_DONE: cond_true = sp_bnext | sp_bprev ? ir[7:4] == ir[3:0]
			     : it == 5'd31;
      default:   cond_true = 1'b0;
    endcase

  // With FAST_INC, an inc adds 2 to ADR[4:1] in parallel, unless that
  // would carry out of bit 4 (one time in 16).  Otherwise it is an
  // ordinary pass.
  wire inc_fast = FAST_INC != 0 && u_kind == KIND_INC && adr[4:1] != 4'hf;

  wire last = state == S_PASS & cnt == (fast ? 5'd15 : 5'd31)
	    | state == S_START & (u_kind == KIND_WAIT
				  | inc_fast & u_dbuf != DBUF_ROT16)
	    | bus & u_kind != KIND_FETCH & wb_ack_i;
  wire [7:0] next0 = {u_next[7:1], u_next[0] | cond_true};
  wire [7:0] next1 = u_next == UA_DISP2 ? disp : next0;
  // An interrupt is taken in place of the next instruction.  Entering
  // an exception clears ie only as the pass ends, so it must not be
  // seen as enabled then.
  wire [7:0] next = next1 == UA_FETCH & irq_i & ie & !sp_exc ? UA_E_IRQ : next1;

  assign rom_addr = rst_i ? UA_FETCH
		  : state == S_DISP ? disp
		  : last ? next
		  : upc;

  // --- Bus -----------------------------------------------------------

  wire writing = u_kind == KIND_WRITE | u_kind == KIND_WRITEB;

  assign wb_cyc_o = bus;
  assign wb_stb_o = bus;
  assign wb_we_o = bus & writing;
  assign wb_adr_o = adr;
  assign wb_sel_o = u_kind == KIND_READB ? 2'b10
		  : u_kind == KIND_WRITEB ? 2'b01
		  : 2'b11;
  assign wb_dat_o = dbuf[31:16];

`ifdef MOXIBIT_TRACE
  // The address and opcode of each instruction, the registers before
  // it runs and the condition codes, as `mox125-iss -T' prints them:
  // see bench/moxibit/lockstep.sh.
  function [31:0] reg_value (input [4:0] r);
    integer i;
    for (i = 0; i < 16; i = i + 1)
      reg_value[2 * i +: 2] = rf.mem[{r, i[3:0]}];
  endfunction

  task trace_insn;
    integer r;
    begin
      $write ("%08x %04x", adr, wb_dat_i);
      for (r = 0; r < 16; r = r + 1)
	$write (" %08x", reg_value (r[4:0]));
      $write (" %02x\n", {cc_eq, cc_lt, cc_gt, cc_ltu, cc_gtu});
    end
  endtask
`endif

  always @(posedge clk_i)
    if (rst_i)
      begin
	state <= S_START;
	upc <= UA_FETCH;
	adr <= BOOT_ADDRESS;
	ie <= 1'b0;
      end
    else
      begin
	if (last)
	  upc <= next;
	// pushm and popm step through their registers in the instruction's
	// B field.
	if (last & (sp_bnext | sp_bprev))
	  ir[3:0] <= ir[3:0] + {{3{sp_bprev}}, 1'b1};

	case (state)
	  S_START:
	    if (inc_fast)
	      begin
		adr[4:1] <= adr[4:1] + 4'd1;
		cnt <= 0;
		fast <= 1'b1;
		if (u_dbuf == DBUF_ROT16)
		  state <= S_PASS;
	      end
	    else if (u_kind == KIND_PASS | u_kind == KIND_INC)
	      begin
		cnt <= 0;
		c_pc <= 0;
		zacc <= 0;
		fast <= 1'b0;
		state <= S_PREP;
	      end
	    else if (bus & wb_ack_i)
	      begin
		if (u_kind == KIND_FETCH)
		  begin
		    ir <= wb_dat_i;
		    state <= S_DISP;
`ifdef MOXIBIT_TRACE
		    trace_insn;
`endif
		  end
		else if (!writing)
		  dbuf[15:0] <= wb_dat_i;
	      end

	  S_DISP:
	    begin
	      upc <= disp;
	      state <= S_START;
	    end

	  S_PREP:
	    state <= S_PASS;

	  S_PASS:
	    begin
	      if (u_adr != ADR_NONE & !fast)
		adr <= {adr_in, adr[31:1]};
	      if (sp_diviter & cnt == 5'd31)
		adr[0] <= adr[1] | fits;
	      if (dbuf_en)
		dbuf <= {dbuf_in, dbuf[31:1]};
	      c_pc <= pc_cout;
	      c_alu <= alu_cout;
	      c_mdu <= mdu_cout;
	      zacc <= zacc | zbit;
	      dly_a <= aq;
	      dly_r <= mdu_sum;
	      if (cnt == (mask_byte ? 5'd7 : 5'd15))
		sx <= u_mask != MASK_NONE ? wd_raw : y_raw;
	      if (sp_shamt)
		begin
		  flag <= cnt != 5'd0 & (flag | sh_bit & cnt >= 5);
		  if (cnt < 5)
		    sh <= {sh_bit ^ (lsl & sh_seen), sh[4:1]};
		  seen <= sh_seen | sh_bit;
		  if (cnt == 5'd31)
		    bi <= ra;
		end
	      if (sp_mul & cnt == 5'd0)
		bi <= dq;
	      if (set_status)
		case (cnt)
		  5'd0: ie <= ra;
		  5'd4: cc_gtu <= ra;
		  5'd5: cc_ltu <= ra;
		  5'd6: cc_gt <= ra;
		  5'd7: cc_lt <= ra;
		  5'd8: cc_eq <= ra;
		  default: ;
		endcase
	      cnt <= cnt + 1;
	      if (fast & cnt == 5'd15)
		state <= S_START;
	      if (cnt == 5'd31)
		begin
		  if (sp_cmp)
		    begin
		      cc_eq <= !(zacc | alu_out);
		      cc_ltu <= !alu_cout;
		      cc_gtu <= alu_cout & (zacc | alu_out);
		      cc_lt <= ra ^ y_raw ? ra : !alu_cout;
		      cc_gt <= (ra ^ y_raw ? !ra : alu_cout) & (zacc | alu_out);
		    end
		  if (sp_caseq)
		    begin
		      cc_eq <= !(zacc | alu_out);
		      {cc_lt, cc_gt, cc_ltu, cc_gtu} <= 4'b0;
		    end
		  if (sp_exc)
		    ie <= 1'b0;
		  if (sp_dsign)
		    begin
		      sa <= ir[8] & ra;
		      sb <= ir[8] & rb;
		    end
		  if (sp_dabs)
		    begin
		      flag <= 1'b0;
		      it <= 0;
		    end
		  if (sp_diviter)
		    begin
		      flag <= fits;
		      it <= it + 1;
		    end
		  state <= S_START;
		end
	    end

	endcase
      end

endmodule // moxibit
