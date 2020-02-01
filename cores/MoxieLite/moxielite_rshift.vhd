-- Implementation of moxielite
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.moxielite_package.all;
 
entity moxielite_rshift is
port
(
	val : in std_logic_vector(31 downto 0);			-- value to shift
	amt : in std_logic_vector(4 downto 0);			-- amount to shift
	fill : in std_logic;							-- what to fill the lhs with
	result : out std_logic_vector(32 downto 0)		-- shifted value
);
end moxielite_rshift;
 
architecture behavior of moxielite_rshift is 

	signal shift1, shift2, shift3, shift4 : std_logic_vector(32 downto 0); 

begin

	shift1 <= val & "0" when amt(0)='0' else fill & val;
	shift2 <= shift1 when amt(1)='0' else (32 downto 31 => fill) & shift1(32 downto 2);
	shift3 <= shift2 when amt(2)='0' else (32 downto 29 => fill) & shift2(32 downto 4);
	shift4 <= shift3 when amt(3)='0' else (32 downto 25 => fill) & shift3(32 downto 8);
	result <= shift4 when amt(4)='0' else (32 downto 17 => fill) & shift4(32 downto 16);

end;
