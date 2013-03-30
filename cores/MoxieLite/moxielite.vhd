-- Implementation of moxielite
LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.numeric_std.ALL;
USE work.moxielite_package.ALL;
use std.textio.all; --  Imports the standard textio package.


ENTITY moxielite IS
	generic
	(
		BOOT_ADDRESS : std_logic_vector(31 downto 0) := x"00001000";
		BIG_ENDIAN : std_logic := '1'
	);
	port
	(
		reset_n : in std_logic;
		clock : in std_logic;
		wait_n : in std_logic;
		addr : out std_logic_vector(31 downto 1);			-- lsb bit always zero
		din : in std_logic_vector(15 downto 0);
		dout : out std_logic_vector(15 downto 0);
		rd_n : out std_logic;
		wr_n : out std_logic;
		wr_h_n : out std_logic;							-- 0 = write upper byte
		wr_l_n : out std_logic;							-- 0 = write lower byte
                debug_o : out std_logic_vector(7 downto 0);
                gdb_i : in std_logic_vector(1 downto 0);
                irq_i : in std_logic
	);
END moxielite;

ARCHITECTURE behavior OF moxielite IS

	signal reset : std_logic;

        signal Adebug_o : std_logic_vector(7 downto 0);

 	-- Program counter
	signal PC : std_logic_vector(31 downto 0) := BOOT_ADDRESS;
	signal PC_plus_2 : std_logic_vector(31 downto 0);
	signal PC_plus_operand_A : std_logic_vector(31 downto 0);

       	-- Debug interface
        signal reg_dump_index : std_logic_vector(4 downto 0);

	-- Load and Store related signals
	signal addr_internal : std_logic_vector(31 downto 0);
	signal addr_reg : std_logic_vector(31 downto 0);
	signal addr_reg_plus_cycle_bytes : std_logic_vector(31 downto 0);
	signal data_reg : std_logic_vector(31 downto 0);
	signal data_bswap : std_logic_vector(31 downto 0);
	signal data_byte_index : std_logic_vector(2 downto 0);
	signal data_byte_count : std_logic_vector(2 downto 0);
	signal data_byte_index_plus_cycle_bytes : std_logic_vector(2 downto 0);
	signal cycle_bytes : std_logic_vector(1 downto 0);

	-- The register file
	type regfile_type is array (0 to 15) of std_logic_vector(31 downto 0);
	signal regfile : regfile_type;

	-- The special register file
	type sregfile_type is array (0 to 16) of std_logic_vector(31 downto 0);
	signal sregfile : sregfile_type;

	-- Stack calcs
	signal sp_plus_offset : std_logic_vector(31 downto 0);
	signal sp_offset : std_logic_vector(31 downto 0);

	-- Instruction decode
	signal instruction : std_logic_vector(15 downto 0);
	signal execute_state : state_type;
	signal aluop : aluop_type;
	signal condition : condition_type;
	signal addrmode : addrmode_type;
	signal instruction_form : instruction_form_type;

	-- State machine
	signal state : state_type := state_reset;
	signal state_next : state_type;
	signal state_resolved : state_type;
	signal has_imm : std_logic;
	signal have_imm : std_logic;
	signal have_deref : std_logic;
	signal imm_reg : std_logic_vector(31 downto 0);	-- The loaded immediate value

	-- Register decoder
	signal reg_A : std_logic_vector(3 downto 0);
	signal reg_B : std_logic_vector(3 downto 0);
	signal reg_A_value : std_logic_vector(31 downto 0);
	signal reg_B_value : std_logic_vector(31 downto 0);
	signal ptr_base : std_logic_vector(31 downto 0);
	signal ptr_offset : std_logic_vector(31 downto 0);
	signal ptr : std_logic_vector(31 downto 0);
	signal ptr_size : std_logic_vector(2 downto 0);
	signal deref_ptr : std_logic;				-- '1' = deref ptr before execute

	-- ALU related signals
	signal operand_A : std_logic_vector(31 downto 0);
	signal operand_B : std_logic_vector(31 downto 0);
	signal alu_result : std_logic_vector(31 downto 0);
	signal alu_ZFlag : std_logic;
	signal alu_CFlag : std_logic;
	signal alu_OFlag : std_logic;
	signal alu_SFlag : std_logic;

	-- Multiplier/divider
	signal alu2_load : std_logic;
	signal alu2_ready : std_logic;
	signal alu2_result : std_logic_vector(63 downto 0);

	-- Flag registers
	signal ZFlag : std_logic;
	signal CFlag : std_logic;
	signal OFlag : std_logic;
	signal SFlag : std_logic;
	signal LFlag : std_logic;				-- synthesized from other flags
	signal ConditionMatch : std_logic;		-- synthesized from instruction and other flags

BEGIN

 	-- Misc continuous assignments
 	reset <= NOT reset_n;
 	PC_plus_2 <= std_logic_vector(unsigned(PC) + 2);
 	PC_plus_operand_A <= std_logic_vector(unsigned(PC) + unsigned(operand_A));
 	addr_reg_plus_cycle_bytes <= std_logic_vector(unsigned(addr_reg) + unsigned(cycle_bytes));
 	data_byte_index_plus_cycle_bytes <= std_logic_vector(unsigned(data_byte_index) + unsigned(cycle_bytes));
 	LFlag <= SFlag xor OFlag; -- less than
 	addr <= addr_internal(31 downto 1);
 	ptr <= std_logic_vector(unsigned(ptr_base) + unsigned(ptr_offset));
 	sp_plus_offset <= std_logic_vector(unsigned(regfile(1)) + unsigned(sp_offset));

 	-- Generate output signals addr, rd_n, wr_n, wr_h and wr_l
 	output_signals : process (state_resolved, PC, ptr, addr_reg, data_bswap, data_byte_index, data_byte_count)
 	begin

		rd_n <= '1';
		wr_n <= '1';
		wr_l_n <= '0';
		wr_h_n <= '0';
		addr_internal <= (others=>'0');
		dout <= (others=>'0');

 		case state_resolved is

 			when state_error =>
 				addr_internal <= PC;

 			when state_execute_brk =>
 				addr_internal <= PC;

                        when state_debug_dump_reg_high =>

                                dout <= regfile(to_integer(unsigned(reg_dump_index(3 downto 0))))(31 downto 16);

                        when state_debug_dump_reg_low =>

                               dout <= regfile(to_integer(unsigned(reg_dump_index(3 downto 0))))(15 downto 0);

                         when state_debug_dump_PC_high =>

                                        dout <= PC(31 downto 16);

                        when state_debug_dump_PC_low =>

                           dout <= PC(15 downto 0);

 			when state_fetch_memcycle | state_fetch_wait =>
 				addr_internal <= PC;
 				rd_n <= '0';

			when state_load_memcycle | state_load_wait =>
				addr_internal <= addr_reg;
				rd_n <= '0';

			when state_store_memcycle | state_store_wait =>
				addr_internal <= addr_reg;
				wr_n <= '0';
				wr_h_n <= '1';
				wr_l_n <= '1';

				-- Ugh! Handle unaligned memory reads
				if data_byte_index(1 downto 0)="00" then

					if addr_reg(0)='0' then

						if data_byte_count="001" then

							-- Aligned only byte
							dout(7 downto 0) <= data_bswap(7 downto 0);
							wr_l_n <= '0';

						else

							-- Aligned first or only word
							dout <= data_bswap(15 downto 0);
							wr_h_n <= '0';
							wr_l_n <= '0';

						end if;
					else

						-- Unaligned first or only byte
						dout(15 downto 8) <= data_bswap(7 downto 0);
						wr_h_n <= '0';

					end if;

				elsif data_byte_index(1 downto 0)="01" then

					-- Must be unaligned to have this index

					if data_byte_count="010" then

						-- Second byte of unaligned word read
						dout(7 downto 0) <= data_bswap(15 downto 8);
						wr_l_n <= '0';

					else

						-- Second and third bytes of unaligned dword read
						dout <= data_bswap(23 downto 8);
						wr_h_n <= '0';
						wr_l_n <= '0';

					end if;

				elsif data_byte_index(1 downto 0)="10" then

					-- Second word of alighed dword read
					dout(15 downto 0) <= data_bswap(31 downto 16);
					wr_h_n <= '0';
					wr_l_n <= '0';

				else

					-- Final byte of unaligned dword read
					dout(7 downto 0) <= data_bswap(31 downto 24);
					wr_l_n <= '0';

				end if;


			when others =>
				null;

 		end case;

 	end process output_signals;

 	-- Calculate the number of bytes to read/write this memory cycle
 	-- Depends on the number of bytes that need to be written, the current byte index
 	-- and whether the current memory operation is aligned or not`
 	calc_cycle_bytes : process(addr_internal, data_byte_count, data_byte_index)
 	begin

 		if addr_internal(0)='1' then

 			-- Non-aligned
 			cycle_bytes <= "01";				-- 1 byte

 		else

 			-- Aligned
 			if data_byte_count="001" then
 				cycle_bytes <= "01";			-- 1 byte

			elsif data_byte_count="010" and data_byte_index="001" then
				cycle_bytes <= "01";

			elsif data_byte_count="100" and data_byte_index="011" then
				cycle_bytes <= "01";

 			else

 				cycle_bytes <= "10";			-- 2 bytes
 			end if;

 		end if;

 	end process calc_cycle_bytes;


 	-- Instruction decoder
	moxielite_decode : entity work.moxielite_decode
		PORT MAP
		(
			Instruction => instruction,
			ExecuteState => execute_state,
			Condition => condition,
			AddrMode => addrmode,
			AluOp => aluop,
			InstructionForm => instruction_form,
			PtrSize => ptr_size
		);

	-- Register A decoder
	reg_A_decoder : process(instruction_form, instruction)
	begin

		case instruction_form is

			when form_1 =>
				reg_A <= instruction(7 downto 4);

			when form_2 =>
				reg_A <= instruction(11 downto 8);

			when others =>
				reg_A <= (others=>'0');

		end case;

	end process reg_A_decoder;

	-- Register B decoder
	reg_B_decoder : process(instruction_form, instruction)
	begin

		case instruction_form is

			when form_1 =>
				reg_B <= instruction(3 downto 0);

			when others =>
				reg_B <= (others=>'0');

		end case;

	end process reg_B_decoder;

	-- Register A multiplexer
	reg_A_multiplexer : process(reg_A, regfile)
	begin

		reg_A_value <= regfile(to_integer(unsigned(reg_A)));

	end process reg_A_multiplexer;

	-- Register B multiplexer
	reg_B_multiplexer : process(reg_B, regfile)
	begin

		reg_B_value <= regfile(to_integer(unsigned(reg_B)));

	end process reg_B_multiplexer;

	-- Operand A decoder
	operand_A_decoder : process(addrmode, reg_A_value, imm_reg, instruction)
	begin
		case addrmode is

			when addrmode_a_lit4 | addrmode_ab | addrmode_a_imm | addrmode_a_immptr | addrmode_a8v =>
				operand_A <= reg_A_value;

			when addrmode_immptr_a | addrmode_imm =>
				operand_A <= imm_reg;

			when addrmode_pcrel =>
				operand_A <= (31 downto 11 => instruction(9)) & instruction(9 downto 0) & "0";

			when others =>
				operand_A <= (others=>'0');

		end case;

	end process operand_A_decoder;

	-- Operand B decoder
	operand_B_decoder : process(addrmode, reg_A_value, reg_B_value, imm_reg, data_bswap, instruction)
	begin
		case addrmode is

			when addrmode_ab | addrmode_aptr_b | addrmode_aptro_b =>
				operand_B <= reg_B_value;

			when addrmode_a_imm =>
				operand_B <= imm_reg;

			when addrmode_a_immptr | addrmode_a_bptr | addrmode_a_bptro=>
				operand_B <= data_bswap;

			when addrmode_immptr_a =>
				operand_B <= reg_A_value;

			when addrmode_a8v =>
				operand_B <= x"000000" & instruction(7 downto 0);

			when addrmode_a_lit4 =>
				operand_B <= x"00000004";

			when others =>
				operand_B <= (others=>'0');

		end case;
	end process operand_B_decoder;

	-- Memory pointer decoder
	ptr_base_decoder : process(addrmode, imm_reg, reg_A_value, reg_B_value)
	begin

		ptr_base <= (others => '0');
		deref_ptr <= '0';

		case addrmode is

			when addrmode_a_immptr =>
				ptr_base <= imm_reg;
				deref_ptr <= '1';

			when addrmode_immptr_a | addrmode_imm =>
				ptr_base <= imm_reg;

			when addrmode_a_bptr | addrmode_a_bptro =>
				ptr_base <= reg_B_value;
				deref_ptr <= '1';

			when addrmode_aptr_b | addrmode_aptro_b =>
				ptr_base <= reg_A_value;

			when others =>
				Null;

		end case;

	end process ptr_base_decoder;

	ptr_offset_decoder : process(addrmode, imm_reg)
	begin
		case addrmode is

			when addrmode_aptro_b | addrmode_a_bptro =>
				ptr_offset <= imm_reg;

			when others =>
				ptr_offset <= (others => '0');


		end case;

	end process ptr_offset_decoder;

	sp_offset_decoder : process(state_resolved)
	begin
		case state_resolved is

			when state_execute_jsr =>
				sp_offset <= x"FFFFFFF8";		-- -8

			when state_execute_jsr_2 =>
				sp_offset <= x"FFFFFFFC";		-- -4

			when state_execute_ret_2 =>
				sp_offset <= x"00000004";		-- 4

			when state_execute_ret_3 =>
				sp_offset <= x"00000008";		-- 8

			when others =>
				sp_offset <= x"00000000";

		end case;
	end process sp_offset_decoder;

	-- The Primary ALU
	moxielite_alu : entity work.moxielite_alu
		PORT MAP
		(
			A => operand_A,
			B => operand_B,
			C => '0',
			Op => aluop,
			R => alu_result,
			ZFlag => alu_ZFlag,
			CFlag => alu_CFlag,
			OFlag => alu_OFlag,
			SFlag => alu_SFlag
		);


	-- The Secondary ALU (multiply/divide/modulus)
	moxielite_alu2 : entity work.moxielite_alu2
		PORT MAP
		(
			clock => clock,
			reset => reset,
			Load => alu2_load,
			A => operand_A,
			B => operand_B,
			AluOp => aluop,
			R => alu2_result,
			Ready => alu2_ready
		);

	alu2_load <= '1' when state_resolved = state_execute_alu2 else '0';


	-- Check if condition matches
	ConditionMatch_process : process(condition, SFlag, ZFlag, CFlag, OFlag, LFlag)
	begin

		case condition is

			when condition_eq  => ConditionMatch <= ZFlag;
			when condition_ne  => ConditionMatch <= NOT ZFlag;
			when condition_lt  => ConditionMatch <= LFlag;
			when condition_gt  => ConditionMatch <= NOT LFlag AND NOT ZFlag;
			when condition_ltu => ConditionMatch <= CFlag;
			when condition_gtu => ConditionMatch <= NOT CFlag AND NOT ZFlag;
			when condition_ge  => ConditionMatch <= NOT LFlag;
			when condition_le  => ConditionMatch <= LFlag OR ZFlag;
			when condition_geu => ConditionMatch <= NOT CFlag;
			when condition_leu => ConditionMatch <= CFlag OR ZFlag;

		end case;

	end process ConditionMatch_process;

	-- Work out if the current instruction is followed by an immediate 4 byte word
	resolve_has_imm : process(addrmode)
	begin

		case addrmode is

			when addrmode_a_imm | addrmode_a_immptr | addrmode_immptr_a | addrmode_aptro_b | addrmode_a_bptro | addrmode_imm =>
				has_imm <= '1';

			when others =>
				has_imm <= '0';

		end case;

	end process resolve_has_imm;

	-- Resolve pseudo states into an actual state
	resolve_state : process(state, execute_state, has_imm, have_imm, deref_ptr, have_deref)
	begin
		case state is

			when state_decode =>

				-- Do we need to read the 4 byte immediate value?
				if has_imm='1' and have_imm='0' then
					state_resolved <= state_read_imm_setup;

				elsif deref_ptr='1' and have_deref='0' then
					state_resolved <= state_deref_ptr_setup;

				else
					state_resolved <= execute_state;

				end if;


			when state_execute =>
				state_resolved <= execute_state;

			when others =>
				state_resolved <= state;

		end case;

	end process resolve_state;

	-- Byte swap the data register
	-- Used to fix up data reads and writes for big endian
	bswap_data_reg : process(data_reg, data_byte_count)
	begin

		if BIG_ENDIAN='1' then

			case data_byte_count is

		 		when "001" =>
		 			data_bswap <= data_reg;

		 		when "010" =>
-- AG FIX		 			data_bswap <= data_reg(23 downto 16) & data_reg(31 downto 24) & data_reg(7 downto 0) & data_reg(15 downto 8);
                                        data_bswap <= data_reg;

				when "100" =>
-- AG FIX	 			data_bswap <= data_reg(7 downto 0) & data_reg(15 downto 8) & data_reg(23 downto 16) & data_reg(31 downto 24);
                                        data_bswap <= data_reg(15 downto 8) & data_reg(7 downto 0) & data_reg(31 downto 24)  & data_reg(23 downto 16);

		 		when others =>
		 			data_bswap <= x"00000000";

		 	end case;

		 else

		 	data_bswap <= data_reg;

		 end if;

	end process bswap_data_reg;


 	-- Main CPU State Machine
 	cpufsm : process(reset_n, clock)
     variable l : line;
 	begin

 		if reset_n='0' then

 			PC <= BOOT_ADDRESS;
 			state <= state_reset;
 			addr_reg <= (others => '0');
 			data_reg <= (others => '0');
 			data_byte_index <= (others => '0');
 			data_byte_count <= (others => '0');
 			regfile <= (others => (others=>'0'));
 			sregfile <= (others => (others=>'0'));
 			instruction <= (others => '0');
 			have_imm <= '0';
 			have_deref <= '0';
 			imm_reg <= (others => '0');
 			ZFlag <= '0';
 			CFlag <= '0';
 			OFlag <= '0';
 			SFlag <= '0';


 		elsif rising_edge(clock) then

                -- Bdebug_o <= PC(15 downto 8);

			-- Handle state
 			case state_resolved is

 				when state_reset =>
 					state <= state_fetch_pre;
                                        debug_o <= "10101010";

 				when state_fetch_pre =>

                                        if gdb_i(1)='0' then
                                          if irq_i='1' then
                                            if sregfile(0)(1 downto 0)="01" then
                                              -- Disable interrupts
                                              sregfile(0)(0) <= '0';
                                              -- Save the "fault address" in sr5.
                                              sregfile(5) <= PC;
                                              -- Jump to the handler in sr1.
                                              PC <= sregfile(1);
                                              -- Set sr2 to "2", indicating IRQ
                                              sregfile(2) <= "00000000000000000000000000000010";
                                              state <= state_fetch_pre;
                                            else
                                              state <= state_fetch_memcycle;
                                            end if;
                                          else
                                            state <= state_fetch_memcycle;
                                          end if;
                                        else
                                          reg_dump_index <= "00000";
                                          state <= state_debug;
                                        end if;
                                        debug_o <= "00000001";

                                when state_debug =>

                                        if gdb_i(1)='0' then
                                          state <= state_fetch_memcycle;
                                        else
                                          if gdb_i(0)='1' then
                                            if (reg_dump_index /= "10000") then
                                              state <= state_debug_dump_reg_high;
                                            else
                                              state <= state_debug_dump_PC_high;
                                            end if;
                                          end if;
                                        end if;

                                when state_debug_dump_reg_high =>

                                        state <= state_debug_dump_reg_low;

                                when state_debug_dump_reg_low =>

                                        state <= state_debug;
                                        reg_dump_index <= std_logic_vector(unsigned(reg_dump_index) + 1);

                                when state_debug_dump_PC_high =>

                                        state <= state_debug_dump_PC_low;

                                when state_debug_dump_PC_low =>

                                        state <= state_fetch_pre; -- to reset
                                                                  -- the count.

 				when state_fetch_memcycle =>

 					state <= state_fetch_wait;
					have_imm <= '0';
					have_deref <= '0';
                                        debug_o <= "00000010";

				when state_fetch_wait =>

 					if wait_n='1' then

						-- Update PC
						PC <= PC_plus_2;

                                              -- AG FIX
						if BIG_ENDIAN='0' then
							instruction <= din(7 downto 0) & din(15 downto 8);
						else
							instruction <= din;
						end if;
 						state <= state_decode;

 					end if;
                                        debug_o <= "00000011";

 				when state_decode  =>
 					-- Pseudo state to resolve loading immediate operand + deref source pointer
                                        debug_o <= "00000100";
 					null;

				when state_read_imm_setup =>

					-- Setup to read a 4 byte immediate value
					addr_reg <= PC;
					data_byte_index <= "000";
					data_byte_count <= "100";
					state_next <= state_latch_imm;
					state <= state_load_memcycle;
                                        debug_o <= "00000101";

 				when state_latch_imm =>

 					-- Finished reading the immediate value
 					-- Store it and continue decoding
 					PC <= addr_reg;
 					have_imm <= '1';
					imm_reg <= data_bswap;
 					state <= state_decode;
                                        debug_o <= "00000110";

				when state_deref_ptr_setup =>

					-- Setup to read 1,2 or 4 bytes from the
					-- instruction's decoded ptr value
					addr_reg <= ptr;
					data_reg <= (others => '0');
					data_byte_index <= "000";
					data_byte_count <= ptr_size;
					state_next <= state_latch_deref;
					state <= state_load_memcycle;
                                        debug_o <= "00000111";

 				when state_latch_deref =>

 					-- Finished reading dereffed value
 					-- Store it and continue
 					have_deref <= '1';
 					state <= state_decode;
                                        debug_o <= "00001000";

 				when state_load_pre =>

 					state <= state_load_memcycle;
                                        debug_o <= "00001001";

 				when state_load_memcycle =>

 					state <= state_load_wait;
                                        debug_o <= "00001010";

 				when state_load_wait =>

					if wait_n='1' then

						-- Update the address register
						addr_reg <= addr_reg_plus_cycle_bytes;

						-- Ugh! Handle unaligned memory reads
						if data_byte_index(1 downto 0)="00" then

							if addr_reg(0)='0' then

								if data_byte_count="001" then

									-- Aligned only byte
									data_reg(7 downto 0) <= din(7 downto 0);

								else

                                                                     -- Aligned first or only word
                                                                    data_reg(15 downto 0) <= din;

								end if;
							else

								-- Unaligned first or only byte
								data_reg(7 downto 0) <= din(15 downto 8);

							end if;

						elsif data_byte_index(1 downto 0)="01" then

							-- Must be unaligned to have this index

							if data_byte_count="010" then

								-- Second byte of unaligned word read
								data_reg(15 downto 8) <= din(7 downto 0);

							else

								-- Second and third bytes of unaligned dword read
								data_reg(23 downto 8) <= din;

							end if;

						elsif data_byte_index(1 downto 0)="10" then

                                        -- Second word of alighed dword read
                                                                    data_reg(31 downto 16) <= din;

						else

							-- Final byte of unaligned dword read
							data_reg(31 downto 24) <= din(7 downto 0);

						end if;

						-- Read more, or return continue to next state
						if data_byte_index_plus_cycle_bytes = data_byte_count then
							state <= state_next;
						else
							data_byte_index <= data_byte_index_plus_cycle_bytes;
							addr_reg <= addr_reg_plus_cycle_bytes;
							state <= state_load_pre;
						end if;

					end if;
                                        debug_o <= "00001011";

				when state_store_pre =>

					state <= state_store_memcycle;
                                        debug_o <= "00001100";

				when state_store_memcycle =>

					state <= state_store_wait;
                                        debug_o <= "00001101";

				when state_store_wait =>

					if wait_n='1' then

						-- Setup next write, or continue to next state
						if data_byte_index_plus_cycle_bytes=data_byte_count then
							state <= state_next;
						else
							data_byte_index <= data_byte_index_plus_cycle_bytes;
							addr_reg <= addr_reg_plus_cycle_bytes;
							state <= state_store_pre;
						end if;

					end if;
                                        debug_o <= "00001110";

				when state_execute =>

					-- pseudo state, never reached, resolves to one of the states below
                                        debug_o <= "00001111";
					Null;

				when state_execute_cmp =>

					-- Compare just needs to store flags
					ZFlag <= alu_ZFlag;
					OFlag <= alu_OFlag;
					SFlag <= alu_SFlag;
					CFlag <= alu_CFlag;
					state <= state_fetch_pre;
                                        debug_o <= "00010000";

				when state_execute_alu  =>

					-- Store the alu result to the destination register
					regfile(to_integer(unsigned(reg_A))) <= alu_result;

					-- Continue with next instruction
					state <= state_fetch_pre;
                                        debug_o <= "00010001";

				when state_execute_store =>

					-- Setup to write operand B to memory
					addr_reg <= ptr;
					data_reg <= operand_B;
					data_byte_index <= "000";
					data_byte_count <= ptr_size;
					state_next <= state_fetch_pre;
					state <= state_store_memcycle;
                                        debug_o <= "00010010";

				when state_execute_push =>

					addr_reg <= alu_result;
					data_reg <= reg_B_value;
					data_byte_index <= "000";
					data_byte_count <= "100";
					state_next <= state_fetch_pre;
					state <= state_store_memcycle;

					regfile(to_integer(unsigned(reg_A))) <= alu_result;
                                        debug_o <= "00010011";

				when state_execute_pop =>

					addr_reg <= reg_A_value;
					data_reg <= (others => '0');
					data_byte_index <= "000";
					data_byte_count <= "100";
					state_next <= state_execute_pop_2;
					state <= state_load_memcycle;

					regfile(to_integer(unsigned(reg_A))) <= alu_result;
                                        debug_o <= "00010100";

				when state_execute_pop_2 =>

					regfile(to_integer(unsigned(reg_B))) <= data_bswap;
					state <= state_fetch_pre;
                                        debug_o <= "00010101";

				when state_execute_nop =>

					-- Do nothing, this should be easy...
					state <= state_fetch_pre;
                                        debug_o <= "00010110";

				when state_execute_bcc =>

					-- Branch on condition
					if ConditionMatch='1' then
						PC <= PC_plus_operand_A(31 downto 1) & '0';
					end if;

					state <= state_fetch_pre;
                                        debug_o <= "00010111";

				when state_execute_jmp =>

					-- Branch unconditionally
					PC <= operand_A(31 downto 1) & '0';
					state <= state_fetch_pre;
                                        debug_o <= "00011000";

				when state_execute_jsr =>

					-- SP -= 8
					regfile(1) <= sp_plus_offset;

					-- Push PC (return address)
					addr_reg <= sp_plus_offset;
					data_reg <= PC;
					data_byte_index <= "000";
					data_byte_count <= "100";
					state_next <= state_execute_jsr_2;
					state <= state_store_memcycle;
                                        debug_o <= "00011001";

				when state_execute_jsr_2 =>

					-- Push FP
					addr_reg <= sp_plus_offset;
					data_reg <= regfile(0);
					data_byte_index <= "000";
					data_byte_count <= "100";
					state_next <= state_fetch_pre;
					state <= state_store_memcycle;

					-- Update FP
					regfile(0) <= sp_plus_offset;		-- FP <= new SP
					regfile(1) <= sp_plus_offset;		-- SP <= new SP

					-- Jump!
					PC <= operand_A(31 downto 1) & '0';
                                        debug_o <= "00011010";

				when state_execute_ret =>

					-- Pop FP
					addr_reg <= regfile(0);
					data_reg <= (others => '0');
					data_byte_index <= "000";
					data_byte_count <= "100";
					state_next <= state_execute_ret_2;
					state <= state_load_memcycle;

					regfile(1) <= regfile(0);		-- SP <= FP
                                        debug_o <= "00011011";

				when state_execute_ret_2 =>

					-- Store FP
					regfile(0) <= data_bswap;


					-- Pop PC
					addr_reg <= sp_plus_offset;
					data_reg <= (others => '0');
					data_byte_index <= "000";
					data_byte_count <= "100";
					state_next <= state_execute_ret_3;
					state <= state_load_memcycle;

					regfile(1) <= sp_plus_offset;		-- SP+=4
                                        debug_o <= "00011100";

				when state_execute_ret_3 =>

					PC <= data_bswap;
					state <= state_fetch_pre;
					regfile(1) <= sp_plus_offset;		-- SP+=8
                                        debug_o <= "00011101";

				when state_execute_alu2 =>

					state <= state_execute_alu2_wait;
                                        debug_o <= "00011110";

				when state_execute_alu2_wait =>

					if alu2_ready = '1' then

						-- Store the result to the destination register
						regfile(to_integer(unsigned(reg_A))) <= alu2_result(31 downto 0);

						-- Continue with next instruction
						state <= state_fetch_pre;

					end if;
                                        debug_o <= "00011111";

				when state_execute_swi =>

                                        -- Save the "fault address" in sr5.
                                        sregfile(5) <= PC;
                                        -- Jump to the handler in sr1.
					PC <= sregfile(1);
                                        -- Set sr2 to "3", indicating SWI
                                        sregfile(2) <= "00000000000000000000000000000011";
                                        -- Store the SWI number in sr3.
                                        sregfile(3) <= operand_A;
					state <= state_fetch_pre;
                                        debug_o <= "00100000";
					null;

 				when state_error =>

				     -- write (l, String'("BAD"));
				     -- writeline (output, l);
 					-- stay in error state
                                        debug_o <= "00100001";
 					Null;

 				when state_execute_brk =>
				     -- write (l, String'("BRK"));
				     -- writeline (output, l);
                                        debug_o <= "00100010";
 					null;

                                when state_execute_bad =>
                                  debug_o <= "00100100";
                                  state <= state;
--                                state <= state_fetch_pre;

                                when state_execute_gsr =>

                                  regfile(to_integer(unsigned(reg_A))) <=
                                    sregfile(to_integer(unsigned(operand_B)));

				  -- Continue with next instruction
				  state <= state_fetch_pre;
                                  debug_o <= "00100101";

                                when state_execute_ssr =>

                                  sregfile(to_integer(unsigned(operand_B))) <= reg_A_value;

				  -- Continue with next instruction
				  state <= state_fetch_pre;
                                  debug_o <= "00010110";

 				when others =>

 					-- What the?
                                  -- state <= state_error;
                                state <= state;
                                        debug_o <= "00100011";

 			end case;
 		end if;

 	end process cpufsm;



END;
