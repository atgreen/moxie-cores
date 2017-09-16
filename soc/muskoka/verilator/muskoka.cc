#include <stdio.h>
#include <verilated.cpp>
#include <verilated_vcd_c.cpp>
#include <verilated_vcd_c.h>
#include <Vmuskoka.h>
#include <Vmuskoka_muskoka.h>
#include <Vmuskoka_moxie.h>
#include <Vmuskoka_cpu_execute.h>
#include <Vmuskoka_cpu_registerfile.h>
#include <Vmuskoka_uart_wb.h>
#include <Vmuskoka_wb_intercon__pi1.h>

char hex_display_to_char (int v)
{
  char c;
  switch (v)
    {
    case 64: c = '0'; break;
    case 121: c = '1'; break;
    case 36: c = '2'; break;
    case 48: c = '3'; break;
    case 25: c = '4'; break;
    case 18: c = '5'; break;
    case 2: c = '6'; break;
    case 120: c = '7'; break;
    case 0: c = '8'; break;
    case 16: c = '9'; break;
    case 8: c = 'A'; break;
    case 3: c = 'B'; break;
    case 39: c = 'C'; break;
    case 33: c = 'D'; break;
    case 6: c = 'E'; break;
    case 14: c = 'F'; break;
    default: c = '*'; break;
    }
  return c;
}
      

int main()
{
  Vmuskoka soc;

  // Before any evaluation, need to know to calculate those signals only used for tracing
  Verilated::traceEverOn(true);
  puts ("Enabling waves...");
  VerilatedVcdC* tfp = new VerilatedVcdC;
  soc.trace (tfp, 99);
  tfp->open ("vlt_dump.vcd");

  soc.rst_i = 1;
  for (int i = 0; i < 10; i++)
    {
      soc.clk_i = 1;
      soc.eval();
      soc.clk_i = 0;
      soc.eval();
    }
  soc.rst_i = 0;

  for (int i = 0; i < 10000000; i++)
    {
      soc.clk_i = 1;
      soc.eval();
      soc.clk_i = 0;
      soc.eval();
      printf ("0x%x[0x%x][0x%x][%d%d%d][v0 %x][v1 %x][rwe1 %d:%d][rwe2 %d:%d][fp %x][sp %x][r0 %x][r1 %x][r2 %x][r3 %x][r4 %x][r5 %x][r6 %x]: %c%c%c%c,%d\n",
	      soc.muskoka->core->stage_execute->PC_i,
	      soc.muskoka->uart->wtf,
	      soc.muskoka->uart->uart_is_transmitting,
      	      soc.muskoka->bus_intercon->slave_0_sel,
      	      soc.muskoka->bus_intercon->slave_1_sel,
      	      soc.muskoka->bus_intercon->slave_2_sel,
	      soc.muskoka->core->regs->value0_i,
	      soc.muskoka->core->regs->value1_i,
	      soc.muskoka->core->regs->write_enable0_i,
	      soc.muskoka->core->regs->reg_write_index0_i,
	      soc.muskoka->core->regs->write_enable1_i,
	      soc.muskoka->core->regs->reg_write_index1_i,
	      soc.muskoka->core->regs->fp_o,
	      soc.muskoka->core->regs->sp_o,
	      soc.muskoka->core->regs->r0,
	      soc.muskoka->core->regs->r1,
	      soc.muskoka->core->regs->r2,
	      soc.muskoka->core->regs->r3,
	      soc.muskoka->core->regs->r4,
	      soc.muskoka->core->regs->r5,
	      soc.muskoka->core->regs->r6,
	      hex_display_to_char(soc.hex0_o),
	      hex_display_to_char(soc.hex1_o),
	      hex_display_to_char(soc.hex2_o),
	      hex_display_to_char(soc.hex3_o),
	      soc.tx_o);
      if (tfp) tfp->flush();
    }
  soc.final();
  if (tfp) tfp->close();
  
  puts ("Done.");
  return 0;
}
