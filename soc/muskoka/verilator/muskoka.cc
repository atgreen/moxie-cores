#include <stdio.h>
#include <verilated.cpp>
#include <Vmuskoka.h>

int main()
{
  Vmuskoka soc;

  soc.rst_i = 1;
  for (int i = 0; i < 10; i++)
    {
      soc.clk_i = 1;
      soc.eval();
      soc.clk_i = 0;
      soc.eval();
    }
  soc.clk_i = 1;
  soc.rst_i = 0;
  soc.eval();
  
  puts ("Done.");
  return 0;
}
