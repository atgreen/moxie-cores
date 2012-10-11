-- Implementation of moxielite
LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.std_logic_unsigned.all;
USE ieee.numeric_std.ALL;
USE work.moxielite_package.ALL;
 
ENTITY moxielite_multiplier IS
	GENERIC
	( 
		WIDTH : integer := 32
	);
	PORT
	(
		clock : in std_logic;
		A : in std_logic_vector(WIDTH-1 downto 0);		-- first arg
		B : in std_logic_vector(WIDTH-1 downto 0);		-- second arg
		R : out std_logic_vector(WIDTH*2-1 downto 0)	-- result
	);
END moxielite_multiplier;
 
ARCHITECTURE behavior OF moxielite_multiplier IS 

	signal negative : std_logic;
	signal R_mul : std_logic_vector(WIDTH*2-1 downto 0);
	signal pipe1 : std_logic_vector(WIDTH*2-1 downto 0);
	signal pipe2 : std_logic_vector(WIDTH*2-1 downto 0);

BEGIN

	-- Do the multiplication
	R_mul <= A * B;

	-- Output result
	R <= pipe2;

	process (clock)
	begin
		if rising_edge(clock) then

			pipe1 <= R_mul;
			pipe2 <= pipe1;

		end if;
	end process;

END;
