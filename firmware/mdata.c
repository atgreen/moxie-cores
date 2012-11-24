int value = 0x55555555;
int bssvalue;

extern volatile short port_7seg_display;
extern volatile short port_uart;

void delay ()
{
  unsigned long c = 500000;
  while (c != 0)
    {
      asm ("nop");
      c--;
    }
}

#if 0
int _write (int fd, char *buf, int len)
{
  int i = 0;
  while (i < len)
    port_uart = buf[i++];
  return len;
}

extern char __bss_end__[];                /* _end is set in the linker command file */
char *__heap_ptr = (char *)&__bss_end__;

/*
 * sbrk -- changes heap size size. Get nbytes more
 *         RAM. We just increment a pointer in what's
 *         left of memory on the board.
 */
char *
_sbrk (nbytes)
     int nbytes;
{
  char *base;
  char *sp;

  base = __heap_ptr;
  __heap_ptr += nbytes;

  return base;
}

char *
_close () { return 0; }

char *
_fstat () { return 0; }

int
_isatty () { return 1; }

int
_lseek () { return 0; }

int
_read () { return 0; }
#endif

int main()
{
  short i = 0;
  //  puts ("Hello World!");
  while (1)
    {
      port_7seg_display = i++;
      delay ();
    }
  return 0;
}
