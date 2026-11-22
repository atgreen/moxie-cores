/* bala_tb.cc - the verilated Bala SoC, with a terminal on its serial port.

   Runs the SoC with whatever program its RAM was built with, and
   prints what it sends on its serial line, decoded at DIVISOR clocks
   to the bit, 8N1.

   With --expect=TEXT it stops, and succeeds, as soon as TEXT has been
   received, and fails if it has not been by the end; that is how
   bala.core's sim target is used as a test.

   usage: bala_tb [--max_clocks=N] [--divisor=N] [--expect=TEXT] [--vcd]

   FuseSoC passes the options as --name=value (see bala.core).  */

#include <getopt.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

#include <string>

#include <verilated.h>
#include <verilated_vcd_c.h>
#include <Vbala.h>

int
main (int argc, char **argv)
{
  uint64_t max_clocks = 5000000, clock = 0;
  unsigned divisor = 434;
  const char *expect = NULL;
  int vcd_wanted = 0, opt;
  static const struct option options[] = {
    {"max_clocks", required_argument, NULL, 'n'},
    {"divisor", required_argument, NULL, 'd'},
    {"expect", required_argument, NULL, 'e'},
    {"vcd", no_argument, NULL, 'v'},
    {NULL, 0, NULL, 0}
  };

  while ((opt = getopt_long (argc, argv, "", options, NULL)) != -1)
    switch (opt)
      {
      case 'n': max_clocks = strtoull (optarg, NULL, 0); break;
      case 'd': divisor = strtoul (optarg, NULL, 0); break;
      case 'e': expect = optarg; break;
      case 'v': vcd_wanted = 1; break;
      default:
	fprintf (stderr, "usage: bala_tb [--max_clocks=N] [--divisor=N] [--expect=TEXT] [--vcd]\n");
	return 2;
      }

  Vbala *soc = new Vbala;
  VerilatedVcdC *vcd = NULL;

  if (vcd_wanted)
    {
      Verilated::traceEverOn (true);
      vcd = new VerilatedVcdC;
      soc->trace (vcd, 99);
      vcd->open ("bala.vcd");
    }

  soc->rx_i = 1;
  soc->rst_i = 1;
  for (int i = 0; i < 10; i++)
    {
      soc->clk_i = 1; soc->eval ();
      soc->clk_i = 0; soc->eval ();
    }
  soc->rst_i = 0;

  /* The serial line: CLOCKS_LEFT until the middle of the next bit to
     sample, BITS sampled of the character so far.  */
  std::string received;
  int receiving = 0, bits = 0;
  unsigned clocks_left = 0, byte = 0;
  int seen = 0;

  while (clock < max_clocks && !seen)
    {
      soc->clk_i = 1;
      soc->eval ();
      if (vcd)
	vcd->dump (2 * clock);

      if (!receiving)
	{
	  if (!soc->tx_o)
	    {
	      receiving = 1;
	      bits = 0;
	      byte = 0;
	      clocks_left = divisor + divisor / 2;
	    }
	}
      else if (--clocks_left == 0)
	{
	  if (bits < 8)
	    {
	      byte |= (unsigned) soc->tx_o << bits;
	      bits++;
	      clocks_left = divisor;
	    }
	  else
	    {
	      /* The stop bit.  */
	      receiving = 0;
	      putchar (byte);
	      fflush (stdout);
	      received += (char) byte;
	      if (expect && received.find (expect) != std::string::npos)
		seen = 1;
	    }
	}

      soc->clk_i = 0;
      soc->eval ();
      if (vcd)
	vcd->dump (2 * clock + 1);
      clock++;
    }

  if (vcd)
    vcd->close ();
  if (expect)
    {
      fprintf (stderr, "\n[%s \"%s\" after %llu clocks]\n",
	       seen ? "received" : "did not receive", expect,
	       (unsigned long long) clock);
      return seen ? 0 : 1;
    }
  fprintf (stderr, "\n[stopped after %llu clocks]\n", (unsigned long long) clock);
  return 0;
}
