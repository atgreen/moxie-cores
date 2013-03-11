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

#define MOXIE_EX_DIV0 0 /* Divide by zero */
#define MOXIE_EX_BAD  1 /* Illegal instruction */
#define MOXIE_EX_IRQ  2 /* Interrupt request */
#define MOXIE_EX_SWI  3 /* Software interrupt */

#define printf(...)

/* Called from our asm code.  Must return the return address.  */
void *__handle_exception (void *faddr, int exc, int code)
{
  switch (exc)
    {
    case MOXIE_EX_DIV0:
      printf("0x%x: DIVIDE BY ZERO EXCEPTION\n", faddr);
      /* faddr is the fault address, and div is 2-bytes long, so the
	 return address is faddr+2.  */
      return faddr + 2; 
      break;
    case MOXIE_EX_BAD:
      printf("0x%x: ILLEGAL INSTRUCTION EXCEPTION\n", faddr);
      return faddr + 2;
      break;
    case MOXIE_EX_IRQ:
      printf("0x%x: INTERRUPT REQUEST %d\n", faddr, code);
      break;
    case MOXIE_EX_SWI:
      printf("0x%x: SOFTWARE INTERRUPT REQUEST %d\n", faddr, code);
      return faddr + 6;
      break;
    default:
      printf("0x%x: UNKNOWN EXCEPTION 0x%x\n", faddr, exc);
      break;
    }
}

void __moxie_exception_handler();

int main()
{
  short i = 0;
  
  // asm("ssr %0, 1" : : "r" (__moxie_exception_handler));

  while (1)
    {
      port_7seg_display = i++;
      delay ();
    }
  return 0;
}
