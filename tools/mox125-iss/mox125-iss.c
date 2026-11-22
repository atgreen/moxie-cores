/* mox125-iss.c - an instruction set simulator for the mox125 core

   Copyright (c) 2026  Anthony Green

   The above named program is free software; you can redistribute it
   and/or modify it under the terms of the GNU General Public License
   version 2 as published by the Free Software Foundation.

   The above named program is distributed in the hope that it will be
   useful, but WITHOUT ANY WARRANTY; without even the implied warranty
   of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.  */

/* This models what the mox125 RTL does, an instruction at a time
   rather than a clock at a time, so that software too big to run on
   the verilated core in reasonable time can be developed.  It follows
   cores/mox125 closely on purpose, down to the instruction cache
   needing to be flushed when a translation changes, so that anything
   that works here has a good chance of working there.  The RTL is the
   reference: where the two disagree, this file is wrong.

   The machine around the core is the "mox125_sim" board:

     RAM          everywhere below 0xF0000000, allocated as touched
     0xF0000000   console; a byte written here is printed
     0xF0000010   writing nonzero requests an interrupt directly (tests)
     0xF0000020   interrupt controller: pending (read)
     0xF0000024   interrupt controller: enable
     0xF0000030   timer: control, write 1 to start
     0xF0000034   timer: period, in clocks
     0xF0000038   timer: write to acknowledge
     0xF000003C   timer: clocks since reset
     0xF00000F0   a write ends the simulation
     0xF0000100   16550 UART, registers four bytes apart

   The timer is interrupt source 0 and the UART source 1.  There are no
   clocks here, so each instruction is counted as CLOCKS_PER_INSN of
   them.

   usage: mox125-iss [-t] [-T] [-n max-insns] [-r] program.elf
     -t  trace every instruction
     -T  before every instruction, print its address and opcode, the
	 registers and the condition codes, one line each, for
	 comparing with bench/moxibit's lockstep trace
     -n  stop after this many instructions
     -r  report the word at 0x0c0ffee0 on exit, as the bench tests do
     -b  each time execution reaches this address (hex), print the
	 argument registers and where the call came from  */

#include <errno.h>
#include <fcntl.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#define CLOCKS_PER_INSN 8

typedef uint32_t u32;
typedef uint16_t u16;
typedef uint8_t u8;

/* --- Memory ---------------------------------------------------------- */

static u8 *pages[1 << 20];

static u8 *
page (u32 addr)
{
  u8 **p = &pages[addr >> 12];
  if (*p == NULL)
    {
      *p = calloc (1, 4096);
      if (*p == NULL)
	{
	  perror ("calloc");
	  exit (1);
	}
    }
  return *p;
}

static u8 ram_read8 (u32 a) { return page (a)[a & 4095]; }
static void ram_write8 (u32 a, u8 v) { page (a)[a & 4095] = v; }
static u16 ram_read16 (u32 a) { return (ram_read8 (a) << 8) | ram_read8 (a + 1); }

/* --- Devices --------------------------------------------------------- */

static uint64_t insns;
static int done;
static int exit_code;
static int report;
static int test_irq;

static u32 pic_pending, pic_enable;
static int timer_running;
static u32 timer_period, count_latch;
static uint64_t timer_next;

/* 16550 */
static u8 uart_ier, uart_lcr, uart_mcr, uart_scr, uart_dll, uart_dlm;
static int uart_thr_irq;
static int uart_rx = -1;

static uint64_t
clocks (void)
{
  return insns * CLOCKS_PER_INSN;
}

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
  if (uart_rx < 0)
    {
      u8 c;
      if (read (0, &c, 1) == 1)
	{
	  uart_rx = c;
	  uart_update_irq ();
	}
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
	v = 0x04;
      else if ((uart_ier & 2) && uart_thr_irq)
	{
	  v = 0x02;
	  uart_thr_irq = 0;
	}
      else
	v = 0x01;
      break;
    case 3: v = uart_lcr; break;
    case 4: v = uart_mcr; break;
    case 5: v = 0x60 | (uart_rx >= 0 ? 1 : 0); break;
    case 6: v = 0xb0; break;
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

/* The bus is 16 bits wide.  A device register is 32 bits, big-endian,
   so its low half is at offset 2.  */

static u16
bus_read16 (u32 a)
{
  if (a < 0xF0000000)
    return ram_read16 (a);
  if (a >= 0xF0000100 && a < 0xF0000120)
    return uart_read ((a - 0xF0000100) >> 2) << 8;
  switch (a)
    {
    case 0xF0000022: return pic_pending & 0xffff;
    case 0xF0000026: return pic_enable & 0xffff;
    case 0xF000003C: count_latch = (u32) clocks (); return count_latch >> 16;
    case 0xF000003E: return count_latch & 0xffff;
    }
  return 0;
}

static void
bus_write (u32 a, int bytes, u16 v)
{
  if (a < 0xF0000000)
    {
      if (bytes == 1)
	ram_write8 (a, v & 0xff);
      else
	{
	  ram_write8 (a, v >> 8);
	  ram_write8 (a + 1, v & 0xff);
	}
      /* The bench tests finish by writing a byte to 0xdead.  */
      if (report && a == 0xdead)
	done = 1;
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
    case 0xF0000010: test_irq = v != 0; break;
    case 0xF0000026: pic_enable = v; break;
    case 0xF0000032:
      timer_running = v != 0;
      timer_next = clocks () + timer_period;
      break;
    case 0xF0000034: timer_period = (timer_period & 0xffff) | ((u32) v << 16); break;
    case 0xF0000036: timer_period = (timer_period & 0xffff0000) | v; break;
    case 0xF000003A: pic_pending &= ~1u; break;
    case 0xF00000F2: done = 1; exit_code = v; break;
    }
}

/* --- The core -------------------------------------------------------- */

enum { FP = 0, SP = 1, R0 = 2 };

enum
{
  CAUSE_DIV0 = 0, CAUSE_UNDEFINED = 1, CAUSE_IRQ = 2, CAUSE_SWI = 3, CAUSE_BRK = 5, CAUSE_ITLB_MISS = 6,
  CAUSE_DTLB_MISS_LOAD = 7, CAUSE_DTLB_MISS_STORE = 8, CAUSE_IPROT = 9,
  CAUSE_DPROT_LOAD = 10, CAUSE_DPROT_STORE = 11, CAUSE_PRIV = 12
};

/* Condition codes, as bits 8:4 of the status register: eq is the top. */
#define CC_EQ 0x10
#define CC_LT 0x08
#define CC_GT 0x04
#define CC_LTU 0x02
#define CC_GTU 0x01

static u32 reg[16];
static u32 pc;
static unsigned cc;
static int sr_ie, sr_um, sr_mmu;
static u32 sr_handler, sr_cause, sr_swi, sr_resume, sr_scratch6, sr_scratch7;
static u32 sr_fault, sr_tlbhi, sr_tlblo, sr_asid, sr_tlb_handler;
static unsigned sr_saved;	/* {cc[4:0], mmu, um, ie} */

struct tlb_entry { int valid, global, user, writable; u32 asid, tag, ppn; };
static struct tlb_entry dtlb[64], itlb[64];

/* The instruction cache: 256 lines of 32 bytes, tagged with the address
   and the mode they were filled in.  */
static struct { int valid; u32 tag; u8 data[32]; } icache[256];

static int trace, lockstep;
static u32 watch_pc;
static int watching;
static int faulted;

static u32
status_word (void)
{
  return (cc << 4) | (sr_mmu << 3) | (sr_um << 2) | sr_ie;
}

static void
exception (unsigned cause, u32 resume)
{
  sr_saved = (cc << 3) | (sr_mmu << 2) | (sr_um << 1) | sr_ie;
  sr_ie = sr_um = sr_mmu = 0;
  sr_cause = cause;
  sr_resume = resume;
  if (cause >= CAUSE_ITLB_MISS && cause <= CAUSE_DTLB_MISS_STORE && sr_tlb_handler)
    pc = sr_tlb_handler;
  else
    pc = sr_handler;
  faulted = 1;
}

static void
fault (unsigned cause, u32 resume, u32 addr)
{
  exception (cause, resume);
  sr_fault = addr;
  sr_tlbhi = (addr & 0xfffff000) | (sr_asid & 0xff);
}

static struct tlb_entry *
tlb_lookup (struct tlb_entry *tlb, u32 vaddr)
{
  struct tlb_entry *e = &tlb[(vaddr >> 12) & 63];
  if (e->valid && e->tag == (vaddr >> 18) && (e->global || e->asid == (sr_asid & 0xff)))
    return e;
  return NULL;
}

static void
tlb_write (struct tlb_entry *tlb)
{
  struct tlb_entry *e = &tlb[(sr_tlbhi >> 12) & 63];
  e->valid = sr_tlblo & 1;
  e->writable = (sr_tlblo >> 1) & 1;
  e->user = (sr_tlblo >> 3) & 1;
  e->global = (sr_tlblo >> 4) & 1;
  e->asid = sr_tlbhi & 0xff;
  e->tag = sr_tlbhi >> 18;
  e->ppn = sr_tlblo >> 12;
}

/* Translate the address of one data bus cycle.  Returns nonzero, having
   taken the exception, if it cannot be done.  */
static int
dxlate (u32 vaddr, int write, u32 insn_pc, u32 *paddr)
{
  struct tlb_entry *e;

  if (!sr_mmu)
    {
      *paddr = vaddr;
      return 0;
    }
  e = tlb_lookup (dtlb, vaddr);
  if (e == NULL)
    {
      fault (write ? CAUSE_DTLB_MISS_STORE : CAUSE_DTLB_MISS_LOAD, insn_pc, vaddr);
      return 1;
    }
  if ((sr_um && !e->user) || (write && !e->writable))
    {
      fault (write ? CAUSE_DPROT_STORE : CAUSE_DPROT_LOAD, insn_pc, vaddr);
      return 1;
    }
  *paddr = (e->ppn << 12) | (vaddr & 4095);
  return 0;
}

/* Data accesses are done as the RTL does them: a 32-bit access is two
   16-bit cycles, each translated on its own.  A fault part way through
   a store leaves the first half written, as it does there.  */

static int
load (u32 vaddr, int bytes, u32 insn_pc, u32 *value)
{
  u32 pa;
  if (dxlate (vaddr, 0, insn_pc, &pa))
    return 1;
  if (bytes == 1)
    *value = bus_read16 (pa) >> 8;
  else if (bytes == 2)
    *value = bus_read16 (pa);
  else
    {
      u32 hi = bus_read16 (pa);
      if (dxlate (vaddr + 2, 0, insn_pc, &pa))
	return 1;
      *value = (hi << 16) | bus_read16 (pa);
    }
  return 0;
}

static int
store (u32 vaddr, int bytes, u32 insn_pc, u32 value)
{
  u32 pa;
  if (dxlate (vaddr, 1, insn_pc, &pa))
    return 1;
  if (bytes == 4)
    {
      bus_write (pa, 2, value >> 16);
      if (dxlate (vaddr + 2, 1, insn_pc, &pa))
	return 1;
      bus_write (pa, 2, value & 0xffff);
    }
  else
    bus_write (pa, bytes, value & 0xffff);
  return 0;
}

/* Make sure the cache line holding VADDR is present, filling it through
   the instruction TLB if not.  */
static u8 *
icache_line (u32 vaddr, u32 insn_pc)
{
  unsigned set = (vaddr >> 5) & 255;
  u32 tag = (vaddr >> 13) | (sr_mmu << 20) | (sr_um << 19);
  u32 line = vaddr & ~31u, pa = line;
  int i;

  if (icache[set].valid && icache[set].tag == tag)
    return icache[set].data;

  if (sr_mmu)
    {
      struct tlb_entry *e = tlb_lookup (itlb, line);
      if (e == NULL)
	{
	  fault (CAUSE_ITLB_MISS, insn_pc, line);
	  return NULL;
	}
      if (sr_um && !e->user)
	{
	  fault (CAUSE_IPROT, insn_pc, line);
	  return NULL;
	}
      pa = (e->ppn << 12) | (line & 4095);
    }
  for (i = 0; i < 32; i += 2)
    {
      u16 w = bus_read16 (pa + i);
      icache[set].data[i] = w >> 8;
      icache[set].data[i + 1] = w & 0xff;
    }
  icache[set].valid = 1;
  icache[set].tag = tag;
  return icache[set].data;
}

/* Fetch the 48 bits at PC.  Like the RTL, this wants all of them even
   when the instruction turns out to be shorter.  */
static int
fetch (u32 at, u16 *opcode, u32 *operand)
{
  u16 w[3];
  int i;
  for (i = 0; i < 3; i++)
    {
      u32 a = at + 2 * i;
      u8 *line = icache_line (a, at);
      if (line == NULL)
	return 1;
      w[i] = (line[a & 31] << 8) | line[(a & 31) + 1];
    }
  *opcode = w[0];
  *operand = ((u32) w[1] << 16) | w[2];
  return 0;
}

/* The shared restoring divider, on magnitudes with the sign applied
   afterwards, as the RTL does it.  */
static u32
divide (u32 a, u32 b, int is_signed, int want_remainder)
{
  u32 q = (is_signed && (a >> 31)) ? -a : a;
  u32 d = (is_signed && (b >> 31)) ? -b : b;
  u32 r = 0;
  int negate = is_signed && (want_remainder ? (a >> 31) : ((a >> 31) ^ (b >> 31)));
  int i;
  for (i = 0; i < 32; i++)
    {
      uint64_t trial = ((uint64_t) r << 1) | (q >> 31);
      int fits = trial >= d;
      r = fits ? (u32) (trial - d) : (u32) trial;
      q = (q << 1) | fits;
    }
  q = want_remainder ? r : q;
  return negate ? -q : q;
}

static u32
gsr (unsigned n)
{
  switch (n & 15)
    {
    case 0: return status_word ();
    case 1: return sr_handler;
    case 2: return sr_cause;
    case 3: return sr_swi;
    case 4: return ((sr_saved >> 1) << 2) | (sr_saved & 1);
    case 5: return sr_resume;
    case 6: return sr_scratch6;
    case 7: return sr_scratch7;
    case 8: return sr_fault;
    case 9: return sr_tlbhi;
    case 10: return sr_tlblo;
    case 12: return sr_asid & 0xff;
    case 13: return sr_tlb_handler;
    }
  return 0;
}

static void
ssr (unsigned n, u32 v)
{
  switch (n & 15)
    {
    case 0: sr_ie = v & 1; cc = (v >> 4) & 31; break;
    case 1: sr_handler = v; break;
    case 2: sr_cause = v & 15; break;
    case 3: sr_swi = v; break;
    case 4: sr_saved = (((v >> 2) & 0x7f) << 1) | (v & 1); break;
    case 5: sr_resume = v; break;
    case 6: sr_scratch6 = v; break;
    case 7: sr_scratch7 = v; break;
    case 8: sr_fault = v; break;
    case 9: sr_tlbhi = v; break;
    case 10: sr_tlblo = v; break;
    case 11: tlb_write ((v & 1) ? itlb : dtlb); break;
    case 12: sr_asid = v & 0xff; break;
    case 13: sr_tlb_handler = v; break;
    case 14: memset (icache, 0, sizeof icache); break;
    }
}

static int
irq_requested (void)
{
  return test_irq || (pic_pending & pic_enable) != 0;
}

static void
step (void)
{
  u16 op;
  u32 operand, next, v, t;
  unsigned a, b, code;
  u32 at = pc;

  faulted = 0;

  if (irq_requested () && sr_ie)
    {
      exception (CAUSE_IRQ, pc);
      return;
    }
  if (fetch (at, &op, &operand))
    return;

  if (watching && at == watch_pc)
    {
      /* At a function's first instruction the frame the call built is
	 at $fp, so the caller can be named.  Kernel addresses only.  */
      u32 f = reg[FP] >= 0xc0000000 ? reg[FP] - 0xc0000000 : reg[FP];
      fprintf (stderr, "[%llu: at %08x from %08x: r0=%08x r1=%08x r2=%08x r3=%08x]\n",
	       (unsigned long long) insns, at,
	       (u32) (ram_read16 (f + 4) << 16) | ram_read16 (f + 6),
	       reg[2], reg[3], reg[4], reg[5]);
    }
  if (trace)
    fprintf (stderr, "%08x: %04x %08x  fp=%08x sp=%08x r0=%08x r1=%08x r2=%08x\n",
	     at, op, operand, reg[FP], reg[SP], reg[2], reg[3], reg[4]);
  if (lockstep)
    {
      int i;
      fprintf (stderr, "%08x %04x", at, op);
      for (i = 0; i < 16; i++)
	fprintf (stderr, " %08x", reg[i]);
      fprintf (stderr, " %02x\n", cc);
    }

  if (op & 0x8000)
    {
      if (op & 0x4000)
	{
	  /* Form 3: conditional branches, bra, and the short ldi.l, whose
	     7-bit value is bit 10, bits 9-8 and bits 3-0.  */
	  int32_t off = op & 0x3ff;
	  if (((op >> 11) & 7) == 6)
	    {
	      int32_t k = ((op >> 4) & 0x70) | (op & 15);
	      reg[(op >> 4) & 15] = (u32) (k >= 64 ? k - 128 : k);
	      pc = at + 2;
	      return;
	    }
	  int take = 0;
	  if (off & 0x200)
	    off -= 0x400;
	  switch ((op >> 10) & 15)
	    {
	    case 0: take = cc & CC_EQ; break;
	    case 1: take = !(cc & CC_EQ); break;
	    case 2: take = cc & CC_LT; break;
	    case 3: take = cc & CC_GT; break;
	    case 4: take = cc & CC_LTU; break;
	    case 5: take = cc & CC_GTU; break;
	    case 6: take = cc & (CC_EQ | CC_GT); break;
	    case 7: take = cc & (CC_EQ | CC_LT); break;
	    case 8: take = cc & (CC_EQ | CC_GTU); break;
	    case 9: take = cc & (CC_EQ | CC_LTU); break;
	    case 10: take = 1; break;		/* bra */
	    default: exception (CAUSE_UNDEFINED, at); return;
	    }
	  pc = take ? at + 2 + off * 2 : at + 2;
	  return;
	}
      /* Form 2: inc, dec, gsr, ssr.  */
      a = (op >> 8) & 15;
      v = op & 0xff;
      switch ((op >> 12) & 3)
	{
	case 0: reg[a] += v; break;
	case 1: reg[a] -= v; break;
	case 2:
	  if (sr_um) { exception (CAUSE_PRIV, at); return; }
	  reg[a] = gsr (v);
	  break;
	case 3:
	  if (sr_um) { exception (CAUSE_PRIV, at); return; }
	  ssr (v, reg[a]);
	  break;
	}
      pc = at + 2;
      return;
    }

  code = op >> 8;
  a = (op >> 4) & 15;
  b = op & 15;
  next = at + 2;

#define DIVIDE(is_signed, want_remainder)				\
  do {									\
    if (reg[b] == 0) { exception (CAUSE_DIV0, at); return; }		\
    reg[a] = divide (reg[a], reg[b], is_signed, want_remainder);	\
  } while (0)
/* ashli, lshri, ashri and cmpi keep bit 4 of their immediate in the
   opcode.  */
#define IMM5 ((code & 1) << 4 | b)
/* The short ldo.l and sto.l: 3-bit register fields for $r0-$r7, and a
   word offset from bits 9, 8, 7 and 3; or $fp less four times one more
   than IMM5.  */
#define A3 (((op >> 4) & 7) + R0)
#define B3 ((op & 7) + R0)
#define OFF4 ((u32) ((op >> 6 & 12) | (op >> 6 & 2) | (op >> 3 & 1)) * 4)
#define FPOFF (reg[FP] - 4 * (IMM5 + 1))
#define OFFSET ((u32) (int32_t) (int16_t) (operand >> 16))
#define LOAD(addr, bytes) do { if (load (addr, bytes, at, &v)) return; } while (0)
#define STORE(addr, bytes, val) do { if (store (addr, bytes, at, val)) return; } while (0)

  switch (code)
    {
    case 0x01: case 0x1b: case 0x20:	/* ldi.l, ldi.b, ldi.s */
      reg[a] = operand; next = at + 6; break;
    case 0x02: reg[a] = reg[b]; break;
    case 0x03: case 0x19:		/* jsra, jsr */
      {
	u32 target = code == 0x03 ? operand : reg[a];
	u32 ret = code == 0x03 ? at + 6 : at + 2;
	u32 sp = reg[SP];
	STORE (sp - 8, 4, ret);
	STORE (sp - 12, 4, reg[FP]);
	reg[SP] = reg[FP] = sp - 12;
	next = target;
      }
      break;
    case 0x04:				/* ret */
      {
	u32 fp = reg[FP], newfp;
	LOAD (fp, 4); newfp = v;
	LOAD (fp + 4, 4);
	reg[SP] = fp + 12;
	reg[FP] = newfp;
	next = v;
      }
      break;
    case 0x05: reg[a] += reg[b]; break;
    case 0x06:				/* push */
      STORE (reg[a] - 4, 4, reg[b]);
      reg[a] -= 4;
      break;
    case 0x07:				/* pop */
      LOAD (reg[a], 4);
      reg[a] += 4;
      reg[b] = v;
      break;
    case 0x08: LOAD (operand, 4); reg[a] = v; next = at + 6; break;
    case 0x09: STORE (operand, 4, reg[a]); next = at + 6; break;
    case 0x0a: LOAD (reg[b], 4); reg[a] = v; break;
    case 0x0b: STORE (reg[a], 4, reg[b]); break;
    case 0x0c: LOAD (reg[b] + OFFSET, 4); reg[a] = v; next = at + 4; break;
    case 0x0d: STORE (reg[a] + OFFSET, 4, reg[b]); next = at + 4; break;
    case 0x0e: case 0x46: case 0x47:	/* cmp, cmpi */
      {
	u32 x = reg[a], y = code == 0x0e ? reg[b] : (u32) ((IMM5 ^ 16) - 16);
	cc = (x == y ? CC_EQ : 0)
	  | ((int32_t) x < (int32_t) y ? CC_LT : 0)
	  | ((int32_t) x > (int32_t) y ? CC_GT : 0)
	  | (x < y ? CC_LTU : 0) | (x > y ? CC_GTU : 0);
      }
      break;
    case 0x0f: break;			/* nop */
    case 0x10: reg[a] = (int32_t) (int8_t) reg[b]; break;
    case 0x11: reg[a] = (int32_t) (int16_t) reg[b]; break;
    case 0x12: reg[a] = reg[b] & 0xff; break;
    case 0x13: reg[a] = reg[b] & 0xffff; break;
    case 0x1a: next = operand; break;	/* jmpa */
    case 0x1c: LOAD (reg[b], 1); reg[a] = v; break;
    case 0x1d: LOAD (operand, 1); reg[a] = v; next = at + 6; break;
    case 0x1e: STORE (reg[a], 1, reg[b] & 0xff); break;
    case 0x1f: STORE (operand, 1, reg[a] & 0xff); next = at + 6; break;
    case 0x21: LOAD (reg[b], 2); reg[a] = v; break;
    case 0x22: LOAD (operand, 2); reg[a] = v; next = at + 6; break;
    case 0x23: STORE (reg[a], 2, reg[b] & 0xffff); break;
    case 0x24: STORE (operand, 2, reg[a] & 0xffff); next = at + 6; break;
    case 0x25: next = reg[a]; break;	/* jmp */
    case 0x26: reg[a] &= reg[b]; break;
    case 0x27: reg[a] = reg[b] >= 32 ? 0 : reg[a] >> reg[b]; break;
    case 0x28: reg[a] = reg[b] >= 32 ? 0 : reg[a] << reg[b]; break;
    case 0x29: reg[a] -= reg[b]; break;
    case 0x2a: reg[a] = -reg[b]; break;
    case 0x2b: reg[a] |= reg[b]; break;
    case 0x2c: reg[a] = ~reg[b]; break;
    case 0x2d:				/* ashr */
      reg[a] = (u32) ((int32_t) reg[a] >> (reg[b] >= 32 ? 31 : reg[b]));
      break;
    case 0x2e: reg[a] ^= reg[b]; break;
    case 0x2f: reg[a] *= reg[b]; break;
    case 0x30:				/* swi */
      sr_swi = operand;
      exception (CAUSE_SWI, at + 6);
      return;
    case 0x31: DIVIDE (1, 0); break;
    case 0x32: DIVIDE (0, 0); break;
    case 0x33: DIVIDE (1, 1); break;
    case 0x34: DIVIDE (0, 1); break;
    case 0x35:				/* brk */
      exception (CAUSE_BRK, at + 2);
      return;
    case 0x36: LOAD (reg[b] + OFFSET, 1); reg[a] = v; next = at + 4; break;
    case 0x37: STORE (reg[a] + OFFSET, 1, reg[b] & 0xff); next = at + 4; break;
    case 0x38: LOAD (reg[b] + OFFSET, 2); reg[a] = v; next = at + 4; break;
    case 0x39: STORE (reg[a] + OFFSET, 2, reg[b] & 0xffff); next = at + 4; break;
    case 0x3a:				/* eret */
      if (sr_um) { exception (CAUSE_PRIV, at); return; }
      cc = sr_saved >> 3;
      sr_mmu = (sr_saved >> 2) & 1;
      sr_um = (sr_saved >> 1) & 1;
      sr_ie = sr_saved & 1;
      next = sr_resume;
      break;
    case 0x3b:				/* wfi */
      if (sr_um) { exception (CAUSE_PRIV, at); return; }
      while (!irq_requested () && !done)
	{
	  /* Nothing will happen until the timer fires or a key arrives,
	     so skip ahead to it.  */
	  if (timer_running && (pic_enable & 1))
	    {
	      if (clocks () < timer_next)
		insns = (timer_next + CLOCKS_PER_INSN - 1) / CLOCKS_PER_INSN;
	      pic_pending |= 1;
	      timer_next += timer_period;
	    }
	  else
	    {
	      uart_poll ();
	      if (!irq_requested ())
		{
		  if (!(pic_enable & 2))
		    {
		      fprintf (stderr, "\n[wfi with nothing able to wake it]\n");
		      done = 1;
		    }
		  else
		    usleep (1000);
		}
	    }
	}
      break;
    case 0x3c:				/* cas.l */
      t = reg[a];
      LOAD (t, 4);
      /* Like the RTL, the condition is set before the write is tried.  */
      cc = v == reg[R0] ? CC_EQ : 0;
      if (cc)
	STORE (t, 4, reg[b]);
      reg[R0] = v;
      break;
    case 0x40: case 0x41: reg[a] <<= IMM5; break; /* ashli */
    case 0x42: case 0x43: reg[a] >>= IMM5; break; /* lshri */
    case 0x44: case 0x45:			/* ashri */
      reg[a] = (u32) ((int32_t) reg[a] >> IMM5);
      break;
    case 0x48: case 0x49: case 0x4a: case 0x4b:	/* ldo.l, short */
      LOAD (reg[B3] + OFF4, 4); reg[A3] = v; break;
    case 0x4c: case 0x4d: case 0x4e: case 0x4f:	/* sto.l, short */
      STORE (reg[A3] + OFF4, 4, reg[B3]); break;
    case 0x50: case 0x51: LOAD (FPOFF, 4); reg[a] = v; break;
    case 0x52: case 0x53: STORE (FPOFF, 4, reg[a]); break;
    case 0x54: reg[a] = OFFSET; next = at + 4; break; /* ldi.l, 16-bit */
    case 0x55: reg[a] = reg[b] + OFFSET; next = at + 4; break; /* lea */
    default:
      exception (CAUSE_UNDEFINED, at);
      return;
    }
  pc = next;
}

/* --- Loading and running --------------------------------------------- */

static u32 be32 (const u8 *p) { return ((u32) p[0] << 24) | (p[1] << 16) | (p[2] << 8) | p[3]; }
static u16 be16 (const u8 *p) { return (p[0] << 8) | p[1]; }

static void
load_elf (const char *name)
{
  FILE *f = fopen (name, "rb");
  u8 eh[52], ph[32];
  u32 phoff;
  int phnum, i;

  if (f == NULL || fread (eh, 1, 52, f) != 52 || memcmp (eh, "\177ELF\1\2", 6) != 0)
    {
      fprintf (stderr, "%s: not a 32-bit big-endian ELF file\n", name);
      exit (1);
    }
  phoff = be32 (eh + 28);
  phnum = be16 (eh + 44);
  for (i = 0; i < phnum; i++)
    {
      u32 off, paddr, filesz, j;
      fseek (f, phoff + i * 32, SEEK_SET);
      if (fread (ph, 1, 32, f) != 32)
	break;
      if (be32 (ph) != 1)	/* PT_LOAD */
	continue;
      off = be32 (ph + 4);
      paddr = be32 (ph + 12);
      filesz = be32 (ph + 16);
      fseek (f, off, SEEK_SET);
      for (j = 0; j < filesz; j++)
	{
	  int c = fgetc (f);
	  if (c == EOF)
	    break;
	  ram_write8 (paddr + j, c);
	}
    }
  fclose (f);
}

int
main (int argc, char **argv)
{
  uint64_t max_insns = 0;
  int opt;

  while ((opt = getopt (argc, argv, "tTn:rb:")) != -1)
    switch (opt)
      {
      case 't': trace = 1; break;
      case 'T': lockstep = 1; break;
      case 'n': max_insns = strtoull (optarg, NULL, 0); break;
      case 'r': report = 1; break;
      case 'b': watch_pc = strtoul (optarg, NULL, 16); watching = 1; break;
      default:
	fprintf (stderr, "usage: %s [-t] [-T] [-n max-insns] [-r] program.elf\n", argv[0]);
	return 2;
      }
  if (optind >= argc)
    {
      fprintf (stderr, "usage: %s [-t] [-T] [-n max-insns] [-r] program.elf\n", argv[0]);
      return 2;
    }
  load_elf (argv[optind]);
  fcntl (0, F_SETFL, fcntl (0, F_GETFL) | O_NONBLOCK);

  pc = 0x1000;
  while (!done && (max_insns == 0 || insns < max_insns))
    {
      step ();
      insns++;
      if (timer_running && clocks () >= timer_next)
	{
	  pic_pending |= 1;
	  timer_next += timer_period;
	}
      if ((insns & 0xffff) == 0)
	uart_poll ();
    }
  if (report)
    {
      printf ("%X\n", (u32) (ram_read16 (0x0c0ffee0) << 16) | ram_read16 (0x0c0ffee2));
      return 0;
    }
  fprintf (stderr, "\n[%s after %llu instructions; pc %08x, status %03x, last cause %u]\n",
	   done ? "exit requested" : "stopped", (unsigned long long) insns,
	   pc, status_word (), sr_cause);
  return exit_code;
}
