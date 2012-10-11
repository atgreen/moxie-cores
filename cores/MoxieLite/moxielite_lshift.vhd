-- Implementation of moxielite
LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.numeric_std.ALL;
USE work.moxielite_package.ALL;
 
ENTITY moxielite_lshift IS
	PORT
	(
		val : in std_logic_vector(31 downto 0);			-- value to shift
		amt : in std_logic_vector(4 downto 0);			-- amount to shift
		result : out std_logic_vector(32 downto 0)		-- shifted value
	);
END moxielite_lshift;
 
ARCHITECTURE behavior OF moxielite_lshift IS 

	signal shift1, shift2, shift3, shift4 : std_logic_vector(32 downto 0); 

BEGIN

	shift1 <= "0" & val when amt(0)='0' else val & "0";
	shift2 <= shift1 when amt(1)='0' else shift1(30 downto 0) & "00";
	shift3 <= shift2 when amt(2)='0' else shift2(28 downto 0) & "0000";
	shift4 <= shift3 when amt(3)='0' else shift3(24 downto 0) & "00000000";
	result <= shift4 when amt(4)='0' else shift4(16 downto 0) & "0000000000000000";

END;
