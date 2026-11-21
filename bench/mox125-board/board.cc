/* board.cc - the verilated mox125 core on the simulated board.

   This is the same board as bench/mox125/zephyr.lisp and
   tools/mox125-iss model (see the latter for the register map), driven
   from C++ so that it is fast enough to boot something as big as
   Linux on the real RTL:

     RAM from address zero
     0xF0000000  console output
     0xF0000020  interrupt controller: pending, enable
     0xF0000030  timer: control, period, acknowledge, count
     0xF00000F0  exit
     0xF0000100  16550 UART, registers four bytes apart

   The timer is interrupt source 0 and the UART source 1.

   usage: board [-n max-clocks] [-m megabytes] [-v from-clock] program.elf  */

#include <elf.h>
#include <fcntl.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include <verilated.h>
#include <verilated_vcd_c.h>
#include <Vmoxie.h>

typedef uint8_t u8;
typedef uint16_t u16;
typedef uint32_t u32;

static u8 *ram;
static u32 ram_size = 64 << 20;

static uint64_t cycle;
static int done, exit_code;

static u32 pic_pending, pic_enable;
static int timer_running;
static u32 timer_period, count_latch;
static uint64_t timer_next;

/* --- 16550 ----------------------------------------------------------- */

static u8 uart_ier, uart_lcr, uart_mcr, uart_scr, uart_dll, uart_dlm;
static int uart_thr_irq;
static int uart_rx = -1;

static void
uart_update_irq (void)
{
  if (((uart_ier & 2) && uart_thr_irq) || ((uart_ier & 1) && uart_rx >= 0))
    pic_pending |= 2;
  else
    pic_pending &= ~2u;
}

static void
uart_poll (void)
{
  u8 c;

  if (uart_rx < 0 && read (0, &c, 1) == 1)
    {
      uart_rx = c;
      uart_update_irq ();
    }
}

static u8
uart_read (unsigned reg)
{
  u8 v = 0;

  switch (reg)
    {
    case 0:
      if (uart_lcr & 0x80)
	v = uart_dll;
      else
	{
	  v = uart_rx >= 0 ? uart_rx : 0;
	  uart_rx = -1;
	}
      break;
    case 1:
      v = (uart_lcr & 0x80) ? uart_dlm : uart_ier;
      break;
    case 2:
      if ((uart_ier & 1) && uart_rx >= 0)
	v = 0xc4;
      else if ((uart_ier & 2) && uart_thr_irq)
	{
	  v = 0xc2;
	  uart_thr_irq = 0;
	}
      else
	v = 0xc1;
      break;
    case 3: v = uart_lcr; break;
    case 4: v = uart_mcr; break;
    case 5: v = 0x60 | (uart_rx >= 0 ? 1 : 0); break;
    case 7: v = uart_scr; break;
    }
  uart_update_irq ();
  return v;
}

static void
uart_write (unsigned reg, u8 v)
{
  switch (reg)
    {
    case 0:
      if (uart_lcr & 0x80)
	uart_dll = v;
      else
	{
	  putchar (v);
	  fflush (stdout);
	  uart_thr_irq = 1;
	}
      break;
    case 1:
      if (uart_lcr & 0x80)
	uart_dlm = v;
      else
	{
	  if ((v & 2) && !(uart_ier & 2))
	    uart_thr_irq = 1;
	  uart_ier = v & 0x0f;
	}
      break;
    case 3: uart_lcr = v; break;
    case 4: uart_mcr = v; break;
    case 7: uart_scr = v; break;
    }
  uart_update_irq ();
}

/* --- The bus ---------------------------------------------------------- */

/* The bus is 16 bits wide.  A device register is 32 bits, big-endian,
   so its low half is at offset 2.  */

static u16
bus_read (u32 a)
{
  if (a < ram_size - 1)
    return (ram[a] << 8) | ram[a + 1];
  if (a >= 0xF0000100 && a < 0xF0000120)
    return uart_read ((a - 0xF0000100) >> 2) << 8;
  switch (a)
    {
    case 0xF0000022: return pic_pending & 0xffff;
    case 0xF0000026: return pic_enable & 0xffff;
    case 0xF000003C: count_latch = (u32) cycle; return count_latch >> 16;
    case 0xF000003E: return count_latch & 0xffff;
    }
  if (a < 0xF0000000)
    {
      fprintf (stderr, "\n[read of unmapped address %08x]\n", a);
      done = 1;
      exit_code = 1;
    }
  return 0;
}

static void
bus_write (u32 a, unsigned sel, u16 v)
{
  if (a < ram_size - 1)
    {
      /* A byte store drives the low lane only.  */
      if (sel == 1)
	ram[a] = v & 0xff;
      else
	{
	  ram[a] = v >> 8;
	  ram[a + 1] = v & 0xff;
	}
      return;
    }
  if (a >= 0xF0000100 && a < 0xF0000120)
    {
      uart_write ((a - 0xF0000100) >> 2, v & 0xff);
      return;
    }
  switch (a)
    {
    case 0xF0000000:
      putchar (v & 0xff);
      fflush (stdout);
      break;
    case 0xF0000026: pic_enable = v; break;
    case 0xF0000032:
      timer_running = v != 0;
      timer_next = cycle + timer_period;
      break;
    case 0xF0000034: timer_period = (timer_period & 0xffff) | ((u32) v << 16); break;
    case 0xF0000036: timer_period = (timer_period & 0xffff0000) | v; break;
    case 0xF000003A: pic_pending &= ~1u; break;
    case 0xF00000F2: done = 1; exit_code = v; break;
    }
}

/* --- Loading ---------------------------------------------------------- */

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
      /* A segment that starts with the file's headers is loaded from
	 where its first section is, as the ISS does: by section.  */
      u32 paddr = be32 (ph.p_paddr), size = be32 (ph.p_filesz);
      if (paddr >= ram_size || size > ram_size - paddr)
	{
	  fprintf (stderr, "%s: segment at %08x does not fit in RAM\n", name, paddr);
	  exit (2);
	}
      fseek (f, be32 (ph.p_offset), SEEK_SET);
      if (fread (ram + paddr, 1, size, f) != size)
	{
	  fprintf (stderr, "%s: short read\n", name);
	  exit (2);
	}
    }
  fclose (f);
}

int
main (int argc, char **argv)
{
  uint64_t max_cycles = ~0ull, trace_from = ~0ull;
  int opt;

  while ((opt = getopt (argc, argv, "n:m:v:")) != -1)
    switch (opt)
      {
      case 'n': max_cycles = strtoull (optarg, NULL, 0); break;
      case 'm': ram_size = strtoul (optarg, NULL, 0) << 20; break;
      case 'v': trace_from = strtoull (optarg, NULL, 0); break;
      default:
      usage:
	fprintf (stderr, "usage: board [-n max-clocks] [-m megabytes] [-v from-clock] program.elf\n");
	return 2;
      }
  if (optind + 1 != argc)
    goto usage;

  ram = (u8 *) calloc (1, ram_size);
  load_elf (argv[optind]);
  fcntl (0, F_SETFL, fcntl (0, F_GETFL) | O_NONBLOCK);

  Vmoxie *cpu = new Vmoxie;
  VerilatedVcdC *vcd = NULL;

  if (trace_from != ~0ull)
    {
      Verilated::traceEverOn (true);
      vcd = new VerilatedVcdC;
      cpu->trace (vcd, 99);
      vcd->open ("board.vcd");
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
  int pending = 0;
  u32 pending_adr = 0;
  int pending_we = 0;

  while (!done && cycle < max_cycles)
    {
      cpu->clk_i = 1;
      cpu->eval ();

      /* Behave like the SoC's slaves: act on a request when it first
	 appears and ack it on the following clock.  */
      if (cpu->wb_stb_o && cpu->wb_cyc_o)
	{
	  u32 adr = cpu->wb_adr_o;
	  int we = cpu->wb_we_o;

	  if (pending && adr == pending_adr && we == pending_we)
	    {
	      cpu->wb_ack_i = 1;
	      pending = 0;
	    }
	  else
	    {
	      cpu->wb_ack_i = 0;
	      pending = 1;
	      pending_adr = adr;
	      pending_we = we;
	      if (we)
		bus_write (adr, cpu->wb_sel_o, cpu->wb_dat_o);
	      else
		cpu->wb_dat_i = bus_read (adr);
	    }
	}
      else
	{
	  cpu->wb_ack_i = 0;
	  pending = 0;
	}

      if (timer_running && cycle >= timer_next)
	{
	  pic_pending |= 1;
	  timer_next += timer_period;
	}
      if ((cycle & 0xffff) == 0)
	uart_poll ();
      cpu->irq_i = (pic_pending & pic_enable) != 0;

      if (vcd && cycle >= trace_from)
	vcd->dump (2 * cycle);
      cpu->clk_i = 0;
      cpu->eval ();
      if (vcd && cycle >= trace_from)
	vcd->dump (2 * cycle + 1);
      cycle++;
    }

  if (vcd)
    vcd->close ();
  fflush (stdout);
  fprintf (stderr, "\n[%s after %llu clocks]\n", done ? "exit requested" : "stopped",
	   (unsigned long long) cycle);
  return exit_code;
}
