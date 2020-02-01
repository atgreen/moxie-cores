-- Implementation of moxielite
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.moxielite_package.all;
 
entity moxielite_alu2 is
port
(
	clock : in std_logic;
	clken : in std_logic;
	reset : in std_logic;
	Load : in std_logic;						-- Set to 1 to start operation
	A : in std_logic_vector(31 downto 0);		-- first arg
	B : in std_logic_vector(31 downto 0);		-- second arg
	AluOp : in aluop_type;						-- operation kind
	R : out std_logic_vector(63 downto 0);		-- result
	Ready : out std_logic						-- Indicates when result is ready
);
end moxielite_alu2;
 
architecture behavior of moxielite_alu2 is 

	type state_type is
	(
		state_idle,
		state_mul_1,
		state_mul_2,
		state_div_1,
		state_div_2,
		state_mod_1,
		state_mod_2
	);

	signal signed : std_logic;
	signal negative : std_logic;
	signal A_abs : std_logic_vector(31 downto 0);
	signal B_abs : std_logic_vector(31 downto 0);
	signal multiplier_R : std_logic_vector(63 downto 0);
	signal state : state_type := state_idle;

	signal divider_load : std_logic;
	signal divider_ready : std_logic;
	signal divider_quotient : std_logic_vector(31 downto 0);
	signal divider_remainder : std_logic_vector(31 downto 0);


	-- Helper to take the absolute value of an operand
	function abs_helper(
			val : in std_logic_vector(31 downto 0);
			is_signed : in  std_logic
			) return std_logic_vector is 
	begin
		if (val(31) and is_signed)='1' then 
			return std_logic_vector(unsigned(not(val)) + 1);
		else 
			return val;
		end if;
	end; 

BEGIN

	-- Is this a signed operation?
	signed <= '1' when AluOp=aluop_mul or AluOp=aluop_div or AluOp=aluop_mod else '0';

	-- Work out if the result is negative
	negative <= '1' when (A(31) xor B(31))='1' and signed='1' else '0';

	-- Take the absolute value of operands (only if it's signed)
	A_abs <= abs_helper(A, signed);
	B_abs <= abs_helper(B, signed);

	Ready <= '1' when state=state_idle else '0';

	-- The multiplier
	moxielite_multiplier : entity work.moxielite_multiplier
		GENERIC MAP
		(
			WIDTH => 32
		)
		PORT MAP
		(
			clock => clock,
			clken => clken,
			A => A_abs,
			B => B_abs,
			R => multiplier_R
		);

	-- The divider
	moxielite_divider : entity work.moxielite_divider
	PORT MAP
	(
		Clock => clock,
		ClkEn => clken,
		Reset => reset,
		Load => divider_load,
		Numerator => A_abs,
		Denominator => B_abs,
		Ready => divider_ready,
		Quotient => divider_quotient,
		Remainder => divider_remainder
	);


	process (clock)
	begin

		if rising_edge(clock) then

			if reset='1' then

				state <= state_idle;
				divider_load <= '0';

			elsif clken = '1' then

				case state is

					when state_idle =>

						if Load='1' then

							if AluOp=aluop_mul or AluOp=aluop_umul then
								state <= state_mul_1;
							elsif AluOp=aluop_div or AluOp=aluop_udiv then
								state <= state_div_1;
								divider_load <= '1';
							else
								state <= state_mod_1;
								divider_load <= '1';
							end if;

						end if;

					when state_mul_1 =>

						state <= state_mul_2;

					when state_mul_2 =>

						if negative='1' then
							R <= std_logic_vector(unsigned(NOT multiplier_R) + 1);
						else
							R <= multiplier_R;
						end if;

						state <= state_idle;

					when state_div_1 =>

						state <= state_div_2;
						divider_load <= '0';

					when state_div_2 =>

						if divider_ready = '1' then

							R(63 downto 32) <= (others=>'0');
							if negative='1' then
								R(31 downto 0) <= std_logic_vector((unsigned(NOT(divider_quotient)) + 1));
							else
								R(31 downto 0) <= divider_quotient;
							end if;

							state <= state_idle;
						end if;

					when state_mod_1 =>

						state <= state_mod_2;
						divider_load <= '0';

					when state_mod_2 =>

						if divider_ready = '1' then

							R(63 downto 32) <= (others=>'0');
							if (A(31) and signed)='1' then
								R(31 downto 0) <= std_logic_vector(unsigned(NOT divider_remainder) + 1);
							else
								R(31 downto 0) <= divider_remainder;
							end if;

							state <= state_idle;
						end if;

				end case;

			end if;

		end if;

	end process;


END;


