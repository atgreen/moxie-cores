library IEEE; 
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;

entity moxielite_divider is
generic
(
	SIZE : INTEGER := 32
);
port
(
	Clock         : in  std_logic;
	ClkEn         : in  std_logic;
	Reset         : in  std_logic;
	Load          : in  std_logic;
	Ready         : out std_logic;
	Numerator     : in  std_logic_vector(SIZE-1 downto 0);
	Denominator   : in  std_logic_vector(SIZE-1 downto 0);
	Quotient      : out std_logic_vector(SIZE-1 downto 0);
	Remainder     : out std_logic_vector(SIZE-1 downto 0)
);
end;
 
 
architecture behaviour of moxielite_divider is

	signal buf: STD_LOGIC_VECTOR((2 * SIZE - 1) downto 0); 
	signal dbuf: STD_LOGIC_VECTOR((SIZE - 1) downto 0); 
	signal sm: INTEGER range 0 to SIZE; 

begin 

	Ready <= '1' when sm=0 else '0';

	Quotient <= buf(SIZE-1 downto 0);           -- buf2
	Remainder <= buf(SIZE*2-1 downto SIZE);     -- buf1

	process(Clock) 
	begin 
		if rising_edge(Clock) then  
			if Reset = '1' then 
				buf <= (others => '0');
				dbuf <= (others =>'0');
				sm <= 0; 
			elsif ClkEn = '1' then
				case sm is 
					when 0 => 
						if Load='1' then
							buf(2*SIZE-1 downto SIZE) <= (others => '0'); 
							buf(SIZE-1 downto 0) <= Numerator; 
							dbuf <= Denominator; 
							sm <= 1; 
						end if;
					when others => 
						if buf((2 * SIZE - 2) downto (SIZE - 1)) >= dbuf then 
							buf(2*SIZE-1 downto SIZE) <= '0' & std_logic_vector(unsigned(buf((2 * SIZE - 3) downto (SIZE - 1))) - unsigned(dbuf((SIZE - 2) downto 0)));
							buf(SIZE-1 downto 0) <= buf((SIZE - 2) downto 0) & '1'; 
						else 
							buf <= buf((2 * SIZE - 2) downto 0) & '0'; 
						end if; 
						if sm /= SIZE then 
							sm <= sm + 1; 
						else 
							sm <= 0; 
						end if; 
				end case; 
			end if; 
		end if;
	end process; 

end behaviour;