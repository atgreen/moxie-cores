/* tb.cc - the verilated moxbit core running a bench test program.

   The program is loaded at the addresses its ELF file gives, and the
   core boots at 0x1000.  As in bench/mox125, a test stores its result
   at 0x0c0ffee0 and ends by writing to 0xdead; the result is printed
   the way `mox125-iss -r' prints it, so the two can be compared.

   Every request waits WAIT clocks before it is acked, on top of the
   one clock that the SoC's slaves take.  Writes to 0xF0000010 drive
   the interrupt line, as in bench/mox125.

   Memory is flat, as in bench/mox125, unless -d says to treat
   0xF0000000 and up as tools/mox125-iss does: as devices, which read
   as zero and ignore writes.  lockstep.sh uses that.

   usage: tb [-w wait] [-n max-clocks] [-d] [-v] program.x  */

#include <elf.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include <unordered_map>

#include <verilated.h>
#include <verilated_vcd_c.h>
#include <Vmoxbit.h>

typedef uint8_t u8;
typedef uint16_t u16;
typedef uint32_t u32;

static std::unordered_map<u32, u8> mem;

static u8 read8 (u32 a) { auto i = mem.find (a); return i == mem.end () ? 0 : i->second; }
static u16 read16 (u32 a) { return (read8 (a) << 8) | read8 (a + 1); }

static u32 be32 (u32 v) { return __builtin_bswap32 (v); }
static u16 be16 (u16 v) { return __builtin_bswap16 (v); }

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
      fseek (f, be32 (ph.p_offset), SEEK_SET);
      for (u32 j = 0; j < size; j++)
	{
	  int c = fgetc (f);
	  if (c == EOF)
	    {
	      fprintf (stderr, "%s: short read\n", name);
	      exit (2);
	    }
	  mem[paddr + j] = c;
	}
    }
  fclose (f);
}

int
main (int argc, char **argv)
{
  uint64_t max_cycles = 1000000, cycle = 0;
  int wait = 0, trace = 0, done = 0, devices = 0, opt;

  while ((opt = getopt (argc, argv, "w:n:dv")) != -1)
    switch (opt)
      {
      case 'w': wait = atoi (optarg); break;
      case 'n': max_cycles = strtoull (optarg, NULL, 0); break;
      case 'd': devices = 1; break;
      case 'v': trace = 1; break;
      default:
      usage:
	fprintf (stderr, "usage: tb [-w wait] [-n max-clocks] [-d] [-v] program.x\n");
	return 2;
      }
  if (optind + 1 != argc)
    goto usage;

  load_elf (argv[optind]);

  Vmoxbit *cpu = new Vmoxbit;
  VerilatedVcdC *vcd = NULL;

  if (trace)
    {
      Verilated::traceEverOn (true);
      vcd = new VerilatedVcdC;
      cpu->trace (vcd, 99);
      vcd->open ("tb.vcd");
    }

  cpu->irq_i = 0;
  cpu->wb_ack_i = 0;
  cpu->rst_i = 1;
  for (int i = 0; i < 10; i++)
    {
      cpu->clk_i = 1; cpu->eval ();
      cpu->clk_i = 0; cpu->eval ();
    }
  cpu->rst_i = 0;

  /* The request we are in the middle of answering, if any.  */
  int pending = 0, waited = 0;
  u32 pending_adr = 0;
  int pending_we = 0;

  while (!done && cycle < max_cycles)
    {
      cpu->clk_i = 1;
      cpu->eval ();

      cpu->wb_ack_i = 0;
      if (cpu->wb_stb_o && cpu->wb_cyc_o)
	{
	  u32 adr = cpu->wb_adr_o;
	  int we = cpu->wb_we_o;

	  if (!pending || adr != pending_adr || we != pending_we)
	    {
	      pending = 1;
	      waited = 0;
	      pending_adr = adr;
	      pending_we = we;
	    }
	  else if (waited++ == wait)
	    {
	      pending = 0;
	      cpu->wb_ack_i = 1;
	      /* mox125's bus, as bench/mox125-board has it: a read is
		 the byte at the address on the high lane and the one
		 after it on the low lane; a byte store drives the low
		 lane only.  */
	      int device = devices && adr >= 0xF0000000;

	      if (we)
		{
		  u16 v = cpu->wb_dat_o;
		  if (device)
		    ;
		  else if (cpu->wb_sel_o == 1)
		    mem[adr] = v & 0xff;
		  else
		    {
		      mem[adr] = v >> 8;
		      mem[adr + 1] = v & 0xff;
		    }
		  /* As in bench/mox125, writes to 0xF0000010 drive the
		     interrupt line.  */
		  if (adr == 0xF0000010)
		    cpu->irq_i = v != 0;
		  if (adr == 0xdead)
		    done = 1;
		}
	      else
		cpu->wb_dat_i = device ? 0 : read16 (adr);
	    }
	}
      else
	pending = 0;

      if (vcd)
	vcd->dump (2 * cycle);
      cpu->clk_i = 0;
      cpu->eval ();
      if (vcd)
	vcd->dump (2 * cycle + 1);
      cycle++;
    }

  if (vcd)
    vcd->close ();
  printf ("%X\n", ((u32) read16 (0x0c0ffee0) << 16) | read16 (0x0c0ffee2));
  fprintf (stderr, "[%s after %llu clocks]\n", done ? "exit" : "stopped",
	   (unsigned long long) cycle);
  return done ? 0 : 1;
}
