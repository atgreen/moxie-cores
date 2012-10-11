-- Test Bench for TestBench
LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.numeric_std.ALL;
USE work.moxielite_package.ALL;
use std.textio.all; --  Imports the standard textio package.
 
ENTITY TestBench IS
END TestBench;
 
ARCHITECTURE behavior OF TestBench IS 
 
	-- Clock period definitions
	constant clock_period : time := 10 ns;

	signal clock : std_logic;
	signal reset_n : std_logic;

	signal cpu_addr : std_logic_vector(31 downto 1);
	signal cpu_din : std_logic_vector(15 downto 0);
	signal cpu_dout : std_logic_vector(15 downto 0);
	signal cpu_rd_n : std_logic;
	signal cpu_wr_n : std_logic;
	signal cpu_wr_h_n : std_logic;
	signal cpu_wr_l_n : std_logic;
	signal cpu_wr_h : std_logic;
	signal cpu_wr_l : std_logic;

	signal mem_rd : std_logic;
	signal mem_wr : std_logic;

	signal ram_dout : std_logic_vector(15 downto 0);
	signal ram_space : std_logic;
	signal ram_wr : std_logic;

	signal rom_dout : std_logic_vector(15 downto 0);
	signal rom_space : std_logic;

BEGIN

	-- CPU Core
	moxielite : entity work.moxielite
		GENERIC MAP
		(
			BIG_ENDIAN => '0'
		)
		PORT MAP
		(
			reset_n => reset_n,
			clock => clock,
			wait_n => '1',
			addr => cpu_addr,
			din => cpu_din,
			dout => cpu_dout,
			rd_n => cpu_rd_n,
			wr_n => cpu_wr_n,
			wr_h_n => cpu_wr_h_n,
			wr_l_n => cpu_wr_l_n
		);

	-- Decode CPU control lines
	mem_rd <= '1' when (cpu_rd_n = '0') else '0';
	mem_wr <= '1' when (cpu_wr_n = '0') else '0';

	-- Multiple input data
	cpu_din <= 
		rom_dout when (mem_rd = '1' and rom_space = '1') else
		ram_dout when (mem_rd = '1' and ram_space = '1') else
		x"0000";

	-- 4k RAM
	cpu_wr_h <= NOT cpu_wr_h_n;
	cpu_wr_l <= NOT cpu_wr_l_n;
	sim_ram : entity work.sim_ram
		GENERIC MAP
		(
			ADDR_WIDTH => 11,
			DATA_WIDTH => 16
		)
		PORT MAP
		(
			clock => clock,
			A => cpu_addr(11 downto 1),
			din => cpu_dout,
			dout => ram_dout,
			wr => ram_wr,
			wr_h => cpu_wr_h,
			wr_l => cpu_wr_l
		);

	-- RAM decode
	ram_space <= '1' when cpu_addr(31 downto 12)=x"00002" else '0';
	ram_wr <= '1' when ram_space='1' and cpu_wr_n='0' else '0';

	-- 4K ROM
	sim_rom : entity work.sim_ram
		GENERIC MAP
		(
			ADDR_WIDTH => 11,
			DATA_WIDTH => 16,
			INIT_FILE => "rom.hex"
		)
		PORT MAP
		(
			clock => clock,
			A => cpu_addr(11 downto 1),
			din => (others => '0'),
			dout => rom_dout,
			wr => '0',
			wr_h => '0',
			wr_l => '0'
		);

	-- ROM decode
	rom_space <= '1' when cpu_addr(31 downto 12)=x"00001" else '0';

	-- Clock process definitions
	clock_process :process
	begin
		clock <= '0';
		wait for clock_period/2;
		clock <= '1';
		wait for clock_period/2;
	end process;


	-- Stimulus process
	stim_proc: process
     variable l : line;
	begin		
	     write (l, String'("Reset"));
	     writeline (output, l);

		reset_n <= '0';
		wait for 100 ns;
		reset_n <= '1';

		wait;

	end process;

END;
