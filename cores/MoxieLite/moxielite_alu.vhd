-- Implementation of moxielite
LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.numeric_std.ALL;
USE work.moxielite_package.ALL;
 
ENTITY moxielite_alu IS
	PORT
	(
		A : in std_logic_vector(31 downto 0);		-- first arg
		B : in std_logic_vector(31 downto 0);		-- second arg
		C : in std_logic;							-- carry flag in
		Op : in aluop_type;							-- operation (aluop_*)

		R : out std_logic_vector(31 downto 0);		-- result
		ZFlag : out std_logic;						-- zero flag out
		CFlag : out std_logic;						-- carry flag out
		OFlag : out std_logic;						-- overflow flag 
		SFlag : out std_logic						-- sign flag
	);
END moxielite_alu;
 
ARCHITECTURE behavior OF moxielite_alu IS 
 
 	signal R_int : std_logic_vector(31 downto 0);

 	signal arg_A : std_logic_vector(31 downto 0);
 	signal arg_B : std_logic_vector(31 downto 0);
 	signal R_add : std_logic_vector(31 downto 0);
 	signal R_add_c : std_logic_vector(32 downto 0);
 	signal not_B : std_logic_vector(31 downto 0);
 	signal CFlag_int : std_logic;
 	signal lshift_result : std_logic_vector(32 downto 0);
 	signal rshift_result : std_logic_vector(32 downto 0);
 	signal rshift_fill : std_logic;

BEGIN

	-- Full Adder with all carry bits
	add32: for i in 0 to 31 generate       
	    R_add(i) <= arg_A(i) xor arg_B(i) xor R_add_c(i);
	    R_add_C(i+1) <= (arg_A(i) and arg_B(i)) or
	                    (arg_A(i) and R_add_c(i)) or
	                    (arg_B(i) and R_add_c(i));
	end generate add32;

	-- Sign extension
	not_B <= NOT B;

	-- Translate inputs, setup for intermediate steps
	process (A, B, C, Op, Not_B)
	begin

		arg_A <= (others=>'0');
		arg_B <= (others=>'0');
		R_add_C(0) <= '0';

		case Op is
			when aluop_add =>
				arg_A <= A;
				arg_B <= B; 
				R_add_C(0) <= '0';

			when aluop_adc =>
				arg_A <= A;
				arg_B <= B;
				R_add_C(0) <= C;

			when aluop_sub =>
				arg_A <= A;
				arg_B <= not_B;
				R_add_C(0) <= '1';

			when aluop_sbb =>
				arg_A <= A;
				arg_B <= not_B;
				R_add_c(0) <= NOT C;

			when aluop_neg =>
				arg_A <= (others=>'0');
				arg_B <= not_B;
				R_add_C(0) <= '1';

			when others =>
				Null;

		end case;

	end process;

	-- Process Step 2
	process (A, B, C, Op, R_add, not_B, lshift_result, rshift_result)
	begin
		case Op is
			when aluop_mov =>
				R_int <= B;
				
			when aluop_add | aluop_adc | aluop_sub | aluop_sbb | aluop_neg =>
				R_int <= R_add;

			when aluop_and =>
				R_int <= A and B;

			when aluop_or =>
				R_int <= A or B;

			when aluop_xor =>
				R_int <= A xor B;

			when aluop_not =>
				R_int <= not_B;

			when aluop_shl =>
				R_int <= lshift_result(31 downto 0);

			when aluop_shr | aluop_sar =>
				R_int <= rshift_result(32 downto 1);

			when aluop_rol =>
				R_int <= A(30 downto 0) & A(31);

			when aluop_ror =>
				R_int <= A(0) & A(31 downto 1);

			when aluop_rcl =>
				R_int <= A(30 downto 0) & C;

			when aluop_rcr =>
				R_int <= C & A(31 downto 1);

                        when aluop_sexb =>
                                R_int <= B(7) & B(7) & B(7) & B(7) &
                                         B(7) & B(7) & B(7) & B(7) &
                                         B(7) & B(7) & B(7) & B(7) &
                                         B(7) & B(7) & B(7) & B(7) &
                                         B(7) & B(7) & B(7) & B(7) &
                                         B(7) & B(7) & B(7) & B(7) &
                                         B(7 downto 0);
                                
                        when aluop_sexs =>
                                R_int <= B(15) & B(15) & B(15) & B(15) &
                                         B(15) & B(15) & B(15) & B(15) &
                                         B(15) & B(15) & B(15) & B(15) &
                                         B(15) & B(15) & B(15) & B(15) &
                                         B(15 downto 0);
                                
                        when aluop_zexb =>
                                R_int <= x"000000" & B(7 downto 0);
                                
                        when aluop_zexs =>
                                R_int <= x"0000" & B(15 downto 0);
                                
			when others =>
				R_int <= (others=>'0');
		end case;
	end process;

	-- Carry flag generation
	process (A, B, C, Op, R_int, R_add, R_add_C)
	begin
		case Op is
			when aluop_add | aluop_adc =>
				CFlag_int <= R_add_C(32);

			when aluop_sub | aluop_sbb =>
				CFlag_int <= NOT R_add_C(32);

			when aluop_neg | aluop_and | aluop_or | aluop_xor =>
				CFlag_int <= '0';

			when aluop_shl | aluop_rcl=>
				CFlag_int <= A(31);

			when aluop_shr | aluop_sar | aluop_rcr=>
				CFlag_int <= A(0);

			when others =>
				CFlag_int <= '1';

		end case;

	end process;

	-- Zero flag 
	process (R_int)
	begin
		if R_int=x"00000000" then
			ZFlag <= '1';
		else
			ZFlag <= '0';
		end if;
	end process;

	moxielite_lshift : entity work.moxielite_lshift
		PORT MAP
		(
			val => A,
			amt => B(4 downto 0),
			result => lshift_result
		);

	moxielite_rshift : entity work.moxielite_rshift
		PORT MAP
		(
			val => A,
			amt => B(4 downto 0),
			fill => rshift_fill,
			result => rshift_result
		);

	rshift_fill <= A(31) when Op=aluop_sar else '0';


	-- Overflow flag
	process (A, B, C, Op, R_add, R_add_C, R_int, CFlag_int)
	begin

		case Op is
			when aluop_add | aluop_adc | aluop_sub | aluop_sbb | aluop_neg =>
				OFlag <= R_add_C(32) xor R_add_C(31);

			when aluop_shl | aluop_rol | aluop_rcl =>
				OFlag <= not ( R_int(31) xor CFlag_int );

			when aluop_sar =>
				OFlag <= '0';

			when aluop_shr =>
				OFlag <= A(31);

			when aluop_ror | aluop_rcr =>
				OFlag <= not ( R_int(31) xor R_int(30) );

			when others =>
				OFlag <= '0';

		end case;
	end process;

	-- Sign flag
	process (R_int)
	begin
		SFlag <= R_int(31);
	end process;


	-- output the result
	CFlag <= CFlag_int;
	R <= R_int;

END;
