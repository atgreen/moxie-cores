LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.numeric_std.ALL;

PACKAGE moxielite_package IS

-- ALU operations
type aluop_type is 
(
	-- Combinational Operations
	-- (implemented by main ALU)
	aluop_none,
	aluop_add,
	aluop_sub,		
	aluop_adc,		
	aluop_sbb,		
	aluop_and,		
	aluop_or, 		
	aluop_xor,		
	aluop_rol,
	aluop_ror,
	aluop_rcl,
	aluop_rcr,
	aluop_shl,
	aluop_shr,
	aluop_sar,
	aluop_not,	
	aluop_neg,
	aluop_mov,
        aluop_sexb,
        aluop_sexs,

	-- Sequential Operations
	-- (implemented by secondary ALU)
	aluop_mul,
	aluop_umul,
	aluop_div,
	aluop_udiv,
	aluop_mod,
	aluop_umod
);		

type state_type is
(
	state_reset,

	-- Instruction fetch stage
	state_fetch_pre,
	state_fetch_memcycle,
	state_fetch_wait,

	-- Instruction decode and fetch immediate stage
	state_decode,
	state_read_imm_setup,
	state_latch_imm,
	state_deref_ptr_setup,
	state_latch_deref,

	-- Load stage
	state_load_pre,
	state_load_memcycle,
	state_load_wait,

	-- Store stage
	state_store_pre,
	state_store_memcycle,
	state_store_wait,

	-- Execute stage
	state_execute,
	state_execute_cmp,
	state_execute_alu,				-- Primary ALU
	state_execute_store,
	state_execute_push,
	state_execute_pop,
	state_execute_pop_2,			-- two stages 
	state_execute_nop,
	state_execute_bcc,
	state_execute_jmp,
	state_execute_jsr,
	state_execute_jsr_2,
	state_execute_ret,
	state_execute_ret_2,
	state_execute_ret_3,
	state_execute_alu2,				-- Secondary ALU (mul/div/mod)
	state_execute_alu2_wait,
	state_execute_swi,
	state_execute_brk,
        state_execute_cas,

	state_execute_bad,

	state_execute_gsr,
	state_execute_ssr,

        state_debug,
        state_debug_dump_reg_high,
        state_debug_dump_reg_low,
        state_debug_dump_PC_high,
        state_debug_dump_PC_low,
        
	-- Auxillary states
	state_error
);


-- The moxie processor's 16-bit instructions come in two forms:
--
--  FORM 1 instructions start with a 0 bit...
--
--    0oooooooaaaabbbb
--    0              F
--
--   ooooooo - form 1 opcode number
--   aaaa    - operand A
--   bbbb    - operand B
--
--  FORM 2 instructions start with bits "10"...
--
--    10ooaaaavvvvvvvv
--    0              F
--
--   oo       - form 2 opcode number
--   aaaa     - operand A
--   vvvvvvvv - 8-bit immediate value
--
--  FORM 3 instructions start with a bits "11"...
--
--    11oooovvvvvvvvvv
--    0              F
--
--   oooo         - form 3 opcode number
--   vvvvvvvvvv   - 10-bit immediate value.  */


type addrmode_type is
(
	addrmode_narg,
	addrmode_imm,    
	addrmode_ab,   
	addrmode_a_lit4,
	addrmode_a_imm,   
	addrmode_a_immptr,   
	addrmode_immptr_a,   
	addrmode_aptr_b,  
	addrmode_a_bptr,  
	addrmode_aptro_b, 
	addrmode_a_bptro, 
	addrmode_a8v, 
	addrmode_pcrel
);

type instruction_form_type is
(
	form_1,
	form_2,
	form_3
);

type condition_type is 
(
	condition_eq,
	condition_ne,
	condition_lt,
	condition_gt,
	condition_ltu,
	condition_gtu,
	condition_ge,
	condition_le,
	condition_geu,
	condition_leu
);


END moxielite_package;
