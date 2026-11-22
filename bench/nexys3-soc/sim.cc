/* sim.cc - the Nexys 3 SoC under Verilator.

   This is soc/muskoka/boards/nexys3/nexys3_soc.v, which is everything
   on the FPGA but the pins and the clock generator, with models of
   what the board connects to it: the cellular RAM, and a terminal on
   the serial port.  The boot loader in the ROM runs as it will on the
   board.

   usage: sim [-n max-clocks] [-s file.srec] [-l program.elf] [-g]
              [-v from-clock]
     -s  send this file to the serial port once the boot loader asks
     -l  put this program in the RAM beforehand, as if it had been
         loaded before the last reset
     -g  type "g" at the boot loader, to start what -l loaded
     -v  write sim.vcd from this clock on

   What the serial port sends is printed.  Standard input goes to the
   serial port, once -s and -g are done.

   The options have long forms too, --max_clocks=N, --srec=FILE,
   --elf=FILE, --go and --vcd_from=N, which is how FuseSoC passes them
   (see nexys3-soc.core).  --expect=TEXT stops the simulation, and
   succeeds, once TEXT has been received, and fails it if TEXT never
   comes.

   The RAM model is strict about the things a real one cares about: a
   read returns rubbish unless the address and strobes have been
   steady for a whole clock, and a write whose address or data moves
   while the write strobe is low is reported.  */

#include <elf.h>
#include <fcntl.h>
#include <getopt.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include <string>

#include <verilated.h>
#include <verilated_vcd_c.h>
#include <Vnexys3_soc.h>

typedef uint8_t u8;
typedef uint16_t u16;
typedef uint32_t u32;

/* Clocks to the bit: the UART counts from its divisor down to zero.  */
#define BIT_CLOCKS (108 + 1)

#define RAM_WORDS (8u << 20)

static u16 *ram;
static uint64_t cycle;
static int problems;

/* --- Loading ---------------------------------------------------------- */

static u32 be32 (u32 v) { return __builtin_bswap32 (v); }
static u16 be16 (u16 v) { return __builtin_bswap16 (v); }

static void
ram_poke (u32 a, u8 v)
{
  u16 *w = &ram[(a >> 1) & (RAM_WORDS - 1)];

  /* The byte at an even address is the upper half of its word.  */
  if (a & 1)
    *w = (*w & 0xff00) | v;
  else
    *w = (*w & 0x00ff) | (v << 8);
}

static void
load_elf (const char *name)
{
  FILE *f = fopen (name, "rb");
  Elf32_Ehdr eh;

  if (f == NULL || fread (&eh, sizeof eh, 1, f) != 1
      || memcmp (eh.e_ident, ELFMAG, SELFMAG) != 0)
    {
      fprintf (stderr, "%s: not an ELF file\n", name);
      exit (2);
    }
  for (int i = 0; i < be16 (eh.e_phnum); i++)
    {
      Elf32_Phdr ph;

      fseek (f, be32 (eh.e_phoff) + i * be16 (eh.e_phentsize), SEEK_SET);
      if (fread (&ph, sizeof ph, 1, f) != 1)
	break;
      if (be32 (ph.p_type) != PT_LOAD || ph.p_filesz == 0)
	continue;
      u32 paddr = be32 (ph.p_paddr), size = be32 (ph.p_filesz);
      if (paddr + size > 2 * RAM_WORDS)
	{
	  fprintf (stderr, "%s: segment at %08x does not fit in RAM\n", name, paddr);
	  exit (2);
	}
      fseek (f, be32 (ph.p_offset), SEEK_SET);
      for (u32 n = 0; n < size; n++)
	ram_poke (paddr + n, fgetc (f));
    }
  fclose (f);
}

static std::string
read_file (const char *name)
{
  FILE *f = fopen (name, "rb");
  std::string s;
  int c;

  if (f == NULL)
    {
      perror (name);
      exit (2);
    }
  while ((c = fgetc (f)) != EOF)
    s.push_back (c);
  fclose (f);
  return s;
}

int
main (int argc, char **argv)
{
  uint64_t max_cycles = ~0ull, trace_from = ~0ull;
  std::string to_send, received;
  const char *expect = NULL;
  int seen = 0, opt;
  static const struct option options[] = {
    {"max_clocks", required_argument, NULL, 'n'},
    {"srec", required_argument, NULL, 's'},
    {"elf", required_argument, NULL, 'l'},
    {"go", no_argument, NULL, 'g'},
    {"vcd_from", required_argument, NULL, 'v'},
    {"expect", required_argument, NULL, 'e'},
    {NULL, 0, NULL, 0}
  };

  ram = (u16 *) calloc (RAM_WORDS, sizeof (u16));

  while ((opt = getopt_long (argc, argv, "n:s:l:gv:", options, NULL)) != -1)
    switch (opt)
      {
      case 'n': max_cycles = strtoull (optarg, NULL, 0); break;
      case 's': to_send += read_file (optarg); break;
      case 'l': load_elf (optarg); break;
      case 'g': to_send += "g"; break;
      case 'v': trace_from = strtoull (optarg, NULL, 0); break;
      case 'e': expect = optarg; break;
      default:
	fprintf (stderr, "usage: sim [-n max-clocks] [-s file.srec] [-l program.elf] [-g] [-v from-clock]\n");
	return 2;
      }

  fcntl (0, F_SETFL, fcntl (0, F_GETFL) | O_NONBLOCK);

  Vnexys3_soc *soc = new Vnexys3_soc;
  VerilatedVcdC *vcd = NULL;

  if (trace_from != ~0ull)
    {
      Verilated::traceEverOn (true);
      vcd = new VerilatedVcdC;
      soc->trace (vcd, 99);
      vcd->open ("sim.vcd");
    }

  soc->rx_i = 1;
  soc->mem_dat_i = 0;
  soc->rst_i = 1;
  for (int i = 0; i < 10; i++)
    {
      soc->clk_i = 1; soc->eval ();
      soc->clk_i = 0; soc->eval ();
    }
  soc->rst_i = 0;

  /* The serial line out of the SoC.  */
  int tx_q = 1, tx_bit = -1, tx_wait = 0, tx_char = 0;
  std::string line;
  int prompted = 0;

  /* The serial line into it.  */
  size_t send_at = 0;
  int rx_bit = -1, rx_wait = 0, rx_char = 0;

  /* The RAM's pins as they were a clock ago.  */
  int cen_q = 1, oen_q = 1, wen_q = 1, ubn_q = 1, lbn_q = 1;
  u32 adr_q = 0;
  u16 dat_q = 0;
  int wen_low_for = 0;

  while (cycle < max_cycles && !seen)
    {
      soc->clk_i = 1;
      soc->eval ();

      /* --- Cellular RAM ---------------------------------------------- */
      int cen = soc->mem_cen_o, oen = soc->mem_oen_o, wen = soc->mem_wen_o;
      u32 adr = soc->mem_adr_o;

      if (!cen && !oen && wen)
	{
	  /* Slower than the clock: no data until the request has been
	     there, unchanged, for a whole clock.  */
	  if (!cen_q && !oen_q && adr_q == adr)
	    soc->mem_dat_i = ram[adr & (RAM_WORDS - 1)];
	  else
	    soc->mem_dat_i = 0xdead;
	}
      else
	soc->mem_dat_i = 0xbeef;

      if (!cen && !wen)
	{
	  if (wen_low_for > 0 && (adr != adr_q || soc->mem_dat_o != dat_q))
	    {
	      fprintf (stderr, "\n[clock %llu: RAM address or data moved during a write]\n",
		       (unsigned long long) cycle);
	      problems++;
	    }
	  if (!soc->mem_dat_oe_o)
	    {
	      fprintf (stderr, "\n[clock %llu: RAM write without data driven]\n",
		       (unsigned long long) cycle);
	      problems++;
	    }
	  wen_low_for++;
	}
      else if (!cen_q && !wen_q)
	{
	  /* The write strobe has just gone away: that is the write.  */
	  u16 *w = &ram[adr_q & (RAM_WORDS - 1)];

	  if (wen_low_for < 2)
	    {
	      fprintf (stderr, "\n[clock %llu: RAM write strobe only %d clock long]\n",
		       (unsigned long long) cycle, wen_low_for);
	      problems++;
	    }
	  if (adr != adr_q || !soc->mem_dat_oe_o || soc->mem_dat_o != dat_q)
	    {
	      fprintf (stderr, "\n[clock %llu: RAM address or data not held after a write]\n",
		       (unsigned long long) cycle);
	      problems++;
	    }
	  if (!ubn_q)
	    *w = (*w & 0x00ff) | (dat_q & 0xff00);
	  if (!lbn_q)
	    *w = (*w & 0xff00) | (dat_q & 0x00ff);
	  wen_low_for = 0;
	}

      cen_q = cen; oen_q = oen; wen_q = wen;
      ubn_q = soc->mem_ubn_o; lbn_q = soc->mem_lbn_o;
      adr_q = adr; dat_q = soc->mem_dat_o;

      /* --- What the SoC sends ---------------------------------------- */
      int tx = soc->tx_o;

      if (tx_bit < 0)
	{
	  if (tx_q && !tx)
	    {
	      tx_bit = 0;
	      tx_char = 0;
	      tx_wait = BIT_CLOCKS + BIT_CLOCKS / 2;
	    }
	}
      else if (--tx_wait == 0)
	{
	  if (tx_bit < 8)
	    {
	      tx_char |= tx << tx_bit;
	      tx_bit++;
	      tx_wait = BIT_CLOCKS;
	    }
	  else
	    {
	      putchar (tx_char);
	      fflush (stdout);
	      received += (char) tx_char;
	      if (expect && received.find (expect) != std::string::npos)
		seen = 1;
	      if (tx_char == '\n')
		{
		  /* The boot loader is ready once it has said what to
		     send it.  */
		  if (line.find ("Send S-records") != std::string::npos)
		    prompted = 1;
		  /* Nothing here can turn the power off, so stop when
		     Linux says it has.  */
		  if (line.find ("MACHINE POWER OFF") != std::string::npos
		      || line.find ("Kernel panic") != std::string::npos)
		    max_cycles = cycle + 100000;
		  line.clear ();
		}
	      else
		line.push_back (tx_char);
	      tx_bit = -1;
	    }
	}
      tx_q = tx;

      /* --- What it is sent ------------------------------------------- */
      if (rx_bit < 0)
	{
	  int c = -1;
	  u8 b;

	  soc->rx_i = 1;
	  if (rx_wait > 0)
	    rx_wait--;
	  else if (prompted)
	    {
	      if (send_at < to_send.size ())
		c = (u8) to_send[send_at++];
	      else if ((cycle & 0x3ff) == 0 && read (0, &b, 1) == 1)
		c = b;
	    }
	  if (c >= 0)
	    {
	      rx_char = c;
	      rx_bit = 0;
	      rx_wait = BIT_CLOCKS;
	      soc->rx_i = 0;	/* start bit */
	    }
	}
      else if (--rx_wait == 0)
	{
	  if (rx_bit < 8)
	    {
	      soc->rx_i = (rx_char >> rx_bit) & 1;
	      rx_bit++;
	      rx_wait = BIT_CLOCKS;
	    }
	  else
	    {
	      soc->rx_i = 1;	/* stop bit, and no more than that */
	      rx_bit = -1;
	      rx_wait = BIT_CLOCKS;
	    }
	}

      if (vcd && cycle >= trace_from)
	vcd->dump (2 * cycle);
      soc->clk_i = 0;
      soc->eval ();
      if (vcd && cycle >= trace_from)
	vcd->dump (2 * cycle + 1);
      cycle++;
    }

  if (vcd)
    vcd->close ();
  fflush (stdout);
  fprintf (stderr, "\n[stopped after %llu clocks, display %04x, %d problem%s with the RAM]\n",
	   (unsigned long long) cycle, soc->display_o, problems, problems == 1 ? "" : "s");
  if (expect)
    fprintf (stderr, "[%s \"%s\"]\n", seen ? "received" : "did not receive", expect);
  return problems != 0 || (expect && !seen);
}
