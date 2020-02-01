-- Implementation of moxielite
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.moxielite_package.all;
 
entity moxielite_multiplier is
generic
( 
	WIDTH : integer := 32
);
port
(
	clock : in std_logic;
	clken : in std_logic;
	A : in std_logic_vector(WIDTH-1 downto 0);		-- first arg
	B : in std_logic_vector(WIDTH-1 downto 0);		-- second arg
	R : out std_logic_vector(WIDTH*2-1 downto 0)	-- result
);
end moxielite_multiplier;
 
architecture behavior of moxielite_multiplier is 

	signal negative : std_logic;
	signal R_mul : std_logic_vector(WIDTH*2-1 downto 0);
	signal pipe1 : std_logic_vector(WIDTH*2-1 downto 0);
	signal pipe2 : std_logic_vector(WIDTH*2-1 downto 0);

begin

	-- Do the multiplication
	R_mul <= std_logic_vector(unsigned(A) * unsigned(B));

	-- Output result
	R <= pipe2;

	process (clock)
	begin
		if rising_edge(clock) then
			if clken = '1' then
				pipe1 <= R_mul;
				pipe2 <= pipe1;
			end if;
		end if;
	end process;

end;
