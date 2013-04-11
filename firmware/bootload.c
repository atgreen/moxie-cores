extern volatile short port_7seg_display;
extern volatile short port_uart;
extern volatile short port_pic;

void delay ()
{
  unsigned long c = 5000000;
  while (c != 0)
    {
      asm ("nop");
      c--;
    }
}

void mx_puts (const char *str)
{
  while (*str)
    {
      port_uart = (short)*str;
      str++;
      delay();
    }
}

#define MOXIE_EX_DIV0 0 /* Divide by zero */
#define MOXIE_EX_BAD  1 /* Illegal instruction */
#define MOXIE_EX_IRQ  2 /* Interrupt request */
#define MOXIE_EX_SWI  3 /* Software interrupt */

#define printf(...)

/* Called from our asm code.  Must return the return address.  */
void *__handle_exception (void *faddr, int exc, int code)
{
  static int c = 0x0;
  static int q = 0;

  switch (exc)
    {
    case MOXIE_EX_DIV0:
      printf("0x%x: DIVIDE BY ZERO EXCEPTION\n", faddr);
      /* faddr is the fault address, and div is 2-bytes long, so the
	 return address is faddr+2.  */
      return faddr + 2; 
    case MOXIE_EX_BAD:
      printf("0x%x: ILLEGAL INSTRUCTION EXCEPTION\n", faddr);
      return faddr + 2;
    case MOXIE_EX_IRQ:
      printf("0x%x: INTERRUPT REQUEST %d\n", faddr, code);
      q++;
      if (q == 4) 
	{
	  q = 0;
	  port_7seg_display = c++;
	}
      // Clear the timer interrupt.
      port_pic = 0;
      return faddr;
    case MOXIE_EX_SWI:
      printf("0x%x: SOFTWARE INTERRUPT REQUEST %d\n", faddr, code);
      return faddr + 6;
    default:
      printf("0x%x: UNKNOWN EXCEPTION 0x%x\n", faddr, exc);
      break;
    }
}

void __moxie_exception_handler();

int main()
{
  int i = 0;

  /* Set the exception handler.  */
  asm("ssr %0, 1" : : "r" (__moxie_exception_handler));

  /* Enable interrupts.  */
  asm("ssr %0, 0" : : "r" (i));

  /* Print out welcome message.  */
  mx_puts ("MOXIE On-Chip Bootloader v1.0\n\r");
  mx_puts ("  Copyright (c) 2013 Anthony Green <green@moxielogic.com>\n\r");
  mx_puts ("\n\r");
  mx_puts ("Waiting for S-Record Download...\n\r");

  while (1)
    {
      delay ();
    }
  return 0;
}
