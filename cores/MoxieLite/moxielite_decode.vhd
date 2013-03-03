-- Implementation of moxielite
LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.numeric_std.ALL;
USE work.moxielite_package.ALL;

ENTITY moxielite_decode IS
	PORT
	(
		Instruction : in std_logic_vector(15 downto 0);
		ExecuteState : out state_type;
		Condition : out condition_type;
		AddrMode : out addrmode_type;
		AluOp : out aluop_type;
		InstructionForm : out instruction_form_type;
		PtrSize : out std_logic_vector(2 downto 0)
	);
END moxielite_decode;

ARCHITECTURE behavior OF moxielite_decode IS
BEGIN

	decode_instruction : process(Instruction)
	begin

		-- Defaults;
		ExecuteState <= state_execute_bad;
		AddrMode <= addrmode_narg;
		Condition <= condition_eq;
		AluOp <= aluop_none;
		InstructionForm <= form_1;
		PtrSize <= "XXX";

		if Instruction(15)='0' then

			-- Form 1
			InstructionForm <= form_1;
			Condition <= condition_eq;
			case Instruction(14 downto 8) is
				when "0000000" =>
					-- bad
					AddrMode <= addrmode_narg;
					ExecuteState <= state_execute_bad;

				when "0000001" =>
					-- ldi.l
					AddrMode <= addrmode_a_imm;
					ExecuteState <= state_execute_alu;
					AluOp <= aluop_mov;

				when "0000010" =>
					-- mov
					AddrMode <= addrmode_ab;
					ExecuteState <= state_execute_alu;
					AluOp <= aluop_mov;

				when "0000011" =>
					-- jsra
					AddrMode <= addrmode_imm;
					ExecuteState <= state_execute_jsr;

				when "0000100" =>
					-- ret
					AddrMode <= addrmode_narg;
					ExecuteState <= state_execute_ret;

				when "0000101" =>
					-- add.l
					AddrMode <= addrmode_ab;
					ExecuteState <= state_execute_alu;
					AluOp <= aluop_add;

				when "0000110" =>
					-- push
					AddrMode <= addrmode_a_lit4;
					AluOp <= aluop_sub;
					ExecuteState <= state_execute_push;

				when "0000111" =>
					-- pop
					AddrMode <= addrmode_a_lit4;
					AluOp <= aluop_add;
					ExecuteState <= state_execute_pop;

				when "0001000" =>
					-- lda.l
					AddrMode <= addrmode_a_immptr;
					ExecuteState <= state_execute_alu;
					AluOp <= aluop_mov;
					PtrSize <= "100";

				when "0001001" =>
					-- sta_l;
					AddrMode <= addrmode_immptr_a;
					ExecuteState <= state_execute_store;
					PtrSize <= "100";

				when "0001010" =>
					-- ld.l;
					AddrMode <= addrmode_a_bptr;
					ExecuteState <= state_execute_alu;
					AluOp <= aluop_mov;
					PtrSize <= "100";

				when "0001011" =>
					-- st.l
					AddrMode <= addrmode_aptr_b;
					ExecuteState <= state_execute_store;
					PtrSize <= "100";

				when "0001100" =>
					-- ldo.l
					AddrMode <= addrmode_a_bptro;
					ExecuteState <= state_execute_alu;
					AluOp <= aluop_mov;
					PtrSize <= "100";

				when "0001101" =>
					-- sto.l
					AddrMode <= addrmode_aptro_b;
					ExecuteState <= state_execute_store;
					PtrSize <= "100";

				when "0001110" =>
					-- sub.l
					AddrMode <= addrmode_ab;
					ExecuteState <= state_execute_cmp;
					AluOp <= aluop_sub;

				when "0001111" =>
					-- nop
					AddrMode <= addrmode_narg;
					ExecuteState <= state_execute_nop;

				when "0011001" =>
					-- jsr
					AddrMode <= addrmode_ab;
					ExecuteState <= state_execute_jsr;

				when "0011010" =>
					-- jmp
					AddrMode <= addrmode_imm;
					ExecuteState <= state_execute_jmp;

				when "0011011" =>
					-- ldi.b
					AddrMode <= addrmode_a_imm;
					ExecuteState <= state_execute_alu;
					AluOp <= aluop_mov;

				when "0011100" =>
					-- ld.b
					AddrMode <= addrmode_a_bptr;
					ExecuteState <= state_execute_alu;
					AluOp <= aluop_mov;
					PtrSize <= "001";

				when "0011101" =>
					-- lda.b
					AddrMode <= addrmode_a_immptr;
					ExecuteState <= state_execute_alu;
					AluOp <= aluop_mov;
					PtrSize <= "001";

				when "0011110" =>
					-- st.b
					AddrMode <= addrmode_aptr_b;
					ExecuteState <= state_execute_store;
					PtrSize <= "001";

				when "0011111" =>
					-- sta.b
					AddrMode <= addrmode_immptr_a;
					ExecuteState <= state_execute_store;
					PtrSize <= "001";

				when "0100000" =>
					-- ldi.s
					AddrMode <= addrmode_a_imm;
					ExecuteState <= state_execute_alu;
					AluOp <= aluop_mov;

				when "0100001" =>
					-- ld.s
					AddrMode <= addrmode_a_bptr;
					ExecuteState <= state_execute_alu;
					AluOp <= aluop_mov;
					PtrSize <= "010";

				when "0100010" =>
					-- lda.s
					AddrMode <= addrmode_a_immptr;
					ExecuteState <= state_execute_alu;
					AluOp <= aluop_mov;
					PtrSize <= "010";

				when "0100011" =>
					-- st.s
					AddrMode <= addrmode_aptr_b;
					ExecuteState <= state_execute_store;
					PtrSize <= "010";

				when "0100100" =>
				 	-- sta.s
					AddrMode <= addrmode_immptr_a;
					ExecuteState <= state_execute_store;
					PtrSize <= "010";

				when "0100101" =>
					-- jmp
					AddrMode <= addrmode_ab;
					ExecuteState <= state_execute_jmp;

				when "0100110" =>
					-- and
					AddrMode <= addrmode_ab;
					ExecuteState <= state_execute_alu;
					AluOp <= aluop_and;

				when "0100111" =>
					-- lshr
					AddrMode <= addrmode_ab;
					ExecuteState <= state_execute_alu;
					AluOp <= aluop_shr;

				when "0101000" =>
					-- ashl
					AddrMode <= addrmode_ab;
					ExecuteState <= state_execute_alu;
					AluOp <= aluop_shl;

				when "0101001" =>
					-- sub.l
					AddrMode <= addrmode_ab;
					ExecuteState <= state_execute_alu;
					AluOp <= aluop_sub;

				when "0101010" =>
					-- neg
					AddrMode <= addrmode_ab;
					ExecuteState <= state_execute_alu;
					AluOp <= aluop_neg;

				when "0101011" =>
					-- or
					AddrMode <= addrmode_ab;
					ExecuteState <= state_execute_alu;
					AluOp <= aluop_or;

				when "0101100" =>
					-- not
					AddrMode <= addrmode_ab;
					ExecuteState <= state_execute_alu;
					AluOp <= aluop_not;

				when "0101101" =>
					-- ashr
					AddrMode <= addrmode_ab;
					ExecuteState <= state_execute_alu;
					AluOp <= aluop_sar;

				when "0101110" =>
					-- xor
					AddrMode <= addrmode_ab;
					ExecuteState <= state_execute_alu;
					AluOp <= aluop_xor;

				when "0101111" =>
					-- mul
					AddrMode <= addrmode_ab;
					ExecuteState <= state_execute_alu2;
					AluOp <= aluop_mul;

				when "0110000" =>
					-- swi
					AddrMode <= addrmode_imm;
					ExecuteState <= state_execute_swi;

				when "0110001" =>
					-- div.l
					AddrMode <= addrmode_ab;
					ExecuteState <= state_execute_alu2;
					AluOp <= aluop_div;

				when "0110010" =>
					-- udiv.l
					AddrMode <= addrmode_ab;
					ExecuteState <= state_execute_alu2;
					AluOp <= aluop_udiv;

				when "0110011" =>
					-- mod.l
					AddrMode <= addrmode_ab;
					ExecuteState <= state_execute_alu2;
					AluOp <= aluop_mod;

				when "0110100" =>
					-- umod.l
					AddrMode <= addrmode_ab;
					ExecuteState <= state_execute_alu2;
					AluOp <= aluop_umod;

				when "0110101" =>
					-- brk
					AddrMode <= addrmode_narg;
					ExecuteState <= state_execute_brk;

				when "0110110" =>
					-- ldo.b
					AddrMode <= addrmode_a_bptro;
					ExecuteState <= state_execute_alu;
					AluOp <= aluop_mov;
					PtrSize <= "001";

				when "0110111" =>
					-- sto.b
					AddrMode <= addrmode_aptro_b;
					ExecuteState <= state_execute_store;
					PtrSize <= "001";

				when "0111000" =>
					-- ldo.s
					AddrMode <= addrmode_a_bptro;
					ExecuteState <= state_execute_alu;
					AluOp <= aluop_mov;
					PtrSize <= "010";

				when "0111001" =>
					-- sto.s
					AddrMode <= addrmode_aptro_b;
					ExecuteState <= state_execute_store;
					PtrSize <= "010"; -- sto_s;

				when "0111010" =>
					-- cas
					AddrMode <= addrmode_aptr_b;
					ExecuteState <= state_execute_cas;
					AluOp <= aluop_mov;
					PtrSize <= "100"; -- cas;

				when others =>
					AddrMode <= addrmode_narg;
					ExecuteState <= state_execute_bad;

			end case;

		elsif Instruction(15 downto 14)="10" then

			-- Form 2
			InstructionForm <= form_2;
			AddrMode <= addrmode_a8v;
			case Instruction(13 downto 12) is

				when "00" =>
					-- inc
					ExecuteState <= state_execute_alu;
					AluOp <= aluop_add;

				when "01" =>
					-- dec
					ExecuteState <= state_execute_alu;
					AluOp <= aluop_sub;

				when "10" =>
					ExecuteState <= state_execute_gsr;

				when "11" =>
					ExecuteState <= state_execute_ssr;

				when others =>
					AddrMode <= addrmode_narg;
					ExecuteState <= state_execute_bad;

			end case;

		else

			-- Form 3
			InstructionForm <= form_3;
			AddrMode <= addrmode_pcrel;
			case Instruction(13 downto 10) is

				when x"0" =>
					-- beq
					ExecuteState <= state_execute_bcc;
					condition <= condition_eq;

				when x"1" =>
					-- bne
					ExecuteState <= state_execute_bcc;
					condition <= condition_ne;

				when x"2" =>
					-- blt
					ExecuteState <= state_execute_bcc;
					condition <= condition_lt;

				when x"3" =>
					-- bgt
					ExecuteState <= state_execute_bcc;
					condition <= condition_gt;

				when x"4" =>
					-- bltu
					ExecuteState <= state_execute_bcc;
					condition <= condition_ltu;

				when x"5" =>
					-- bgtu
					ExecuteState <= state_execute_bcc;
					condition <= condition_gtu;

				when x"6" =>
					-- bge
					ExecuteState <= state_execute_bcc;
					condition <= condition_ge;

				when x"7" =>
					-- ble
					ExecuteState <= state_execute_bcc;
					condition <= condition_le;

				when x"8" =>
					-- bgeu
					ExecuteState <= state_execute_bcc;
					condition <= condition_geu;

				when x"9" =>
					-- bleu
					ExecuteState <= state_execute_bcc;
					condition <= condition_leu;

				when others =>
					ExecuteState <= state_execute_bad;

			end case;


		end if;

	end process decode_instruction;

END;
