-- Wrapper around moxielite to present old interface

LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.numeric_std.ALL;
USE work.moxielite_package.ALL;


ENTITY moxielite_classic IS
  generic
    (
      BOOT_ADDRESS : std_logic_vector(31 downto 0) := x"00001000";
      BIG_ENDIAN : std_logic := '1'
      );
  port
    (
      reset_n : in std_logic;
      clock : in std_logic;
      wait_n : in std_logic;
      addr : out std_logic_vector(31 downto 1);			-- lsb bit always zero
      din : in std_logic_vector(15 downto 0);
      dout : out std_logic_vector(15 downto 0);
      rd_n : out std_logic;
      wr_n : out std_logic;
      wr_h_n : out std_logic;							-- 0 = write upper byte
      wr_l_n : out std_logic;							-- 0 = write lower byte
      debug_o : out std_logic_vector(7 downto 0);
      gdb_i : in std_logic_vector(1 downto 0);
      irq_i : in std_logic;
      buserr_i : in std_logic
      );
END moxielite_classic;

ARCHITECTURE behavior OF moxielite_classic IS

    signal s_reset : std_logic;
    signal s_wait : std_logic;
    signal s_rd : std_logic;
    signal s_wr : std_logic;
    signal s_mask : std_logic_vector;
    signal s_addr : std_logic_vector(31 downto 0);

begin

    s_reset <= not reset_n;
    s_wait <= not wait_n;
    wr_h_n <= not s_mask(0);
    wr_l_n <= not s_mask(1);
    addr <= s_addr(31 downto 1);

    cpu : entity work.moxielite
    generic map
    (
        p_boot_address => BOOT_ADDRESS,
        p_big_endian => BIG_ENDIAN
    );
    port map
    (
        i_reset => s_reset,
        i_clock => clock,
        i_clken => '1',
        i_wait => s_wait,
        o_addr => s_addr,
        i_din => din,
        o_dout => dout,
        o_rd => s_rd,
        o_wr => s_wr,
        o_mask => s_mask,
        o_debug => debug_o,
        i_gdb => gdb_i,
        i_irq => irq_i,
        i_buserr => buserr_i
    );


END;
