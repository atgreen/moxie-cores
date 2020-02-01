-- implementation of moxielite
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.moxielite_package.all;
 
entity moxielite_lshift is
port
(
	val : in std_logic_vector(31 downto 0);			-- value to shift
	amt : in std_logic_vector(4 downto 0);			-- amount to shift
	result : out std_logic_vector(32 downto 0)		-- shifted value
);
end moxielite_lshift;
 
architecture behavior of moxielite_lshift is 

	signal shift1, shift2, shift3, shift4 : std_logic_vector(32 downto 0); 

begin

	shift1 <= "0" & val when amt(0)='0' else val & "0";
	shift2 <= shift1 when amt(1)='0' else shift1(30 downto 0) & "00";
	shift3 <= shift2 when amt(2)='0' else shift2(28 downto 0) & "0000";
	shift4 <= shift3 when amt(3)='0' else shift3(24 downto 0) & "00000000";
	result <= shift4 when amt(4)='0' else shift4(16 downto 0) & "0000000000000000";

end;
