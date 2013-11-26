#include <stdio.h>

extern volatile short port_7seg_display;
extern volatile short port_uart[4];
extern volatile short port_pic;

int _write (int fd, char *buf, int len)
{
  int i = 0;
  port_7seg_display = 0x2222;
  
  while ((i < len) && (buf[i] != 0))
    {
      /* Wait for UART to be ready to transmit.  */
      while (port_uart[1] != 1)
	{
	  asm ("" : : : "memory");
	}
      port_7seg_display = (i<<8) + buf[i];
      port_uart[3] = buf[i++];
    }
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

void
_exit (int v) { while (1); }

int main()
{
  void *reset = (void *)0x1000;  // Start of the on-chip bootloader.

  puts ("Hello World!\n");
  fflush(stdout);
  goto *reset;
}
