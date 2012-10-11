-- Entity sim_ram

library ieee;
use ieee.std_logic_1164.ALL;
use ieee.numeric_std.ALL;
use std.textio.all;
use ieee.std_logic_textio.all;

entity sim_ram is
	generic
	(
		ADDR_WIDTH : integer;
		DATA_WIDTH : integer := 8;
		INIT_FILE : string := ""
	);
	port
	(
		clock : in std_logic;
		A : in std_logic_vector(ADDR_WIDTH-1 downto 0);
		din : in std_logic_vector(DATA_WIDTH-1 downto 0);
		dout : out std_logic_vector(DATA_WIDTH-1 downto 0);
		wr : in std_logic;
		wr_h : in std_logic;
		wr_l : in std_logic
	);
end sim_ram;
 
architecture behavior of sim_ram is 
	constant MEM_DEPTH : integer := 2**ADDR_WIDTH-1;
	type mem_type is array(0 to MEM_DEPTH) of std_logic_vector(DATA_WIDTH-1 downto 0);

	impure function InitRamFromFile(RamFileName : in string) return mem_type is
		FILE InitFile : text;
		variable InitFileLine : line;
		variable RAM : mem_type;
	begin
		if INIT_FILE/="" then
			file_open(InitFile, RamFileName, read_mode);
			for I in mem_type'range loop
				readline(InitFile, InitFileLine);
				hread(InitFileLine, RAM(I));
			end loop;
		end if;
		return RAM;
	end function;

	signal ram : mem_type := InitRamFromFile(INIT_FILE);

begin

	process (clock)
	begin

		if rising_edge(clock) then

			if wr = '1' then
				if wr_h='1' then
					ram(to_integer(unsigned(A)))(DATA_WIDTH-1 downto DATA_WIDTH/2) <= din(DATA_WIDTH-1 downto DATA_WIDTH/2);
				end if;
				if wr_l='1' then
					ram(to_integer(unsigned(A)))(DATA_WIDTH/2-1 downto 0) <= din(DATA_WIDTH/2-1 downto 0);
				end if;
			else
				dout <= ram(to_integer(unsigned(A)));
			end if;

		end if;

	end process;

end;
