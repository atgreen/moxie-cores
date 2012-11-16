int value = 0x55555555;
int bssvalue;

extern volatile short port_7seg_display;

void delay ()
{
  unsigned long c = 500000;
  while (c != 0)
    {
      asm ("nop");
      c--;
    }
}

int main()
{
  short i = 0;
  while (1)
    {
      port_7seg_display = i++;
      delay ();
    }
  return 0;
}
