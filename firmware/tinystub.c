/* tinystub.c - a really tiny gdb remote protocol stub for moxie.

   Copyright (c) 2013, Anthony Green

   Redistribution and use in source and binary forms, with or without
   modification, are permitted provided that the following conditions
   are met:

 	Redistributions of source code must retain the above copyright
	notice, this list of conditions and the following disclaimer.

 	Redistributions in binary form must reproduce the above
	copyright notice, this list of conditions and the following
	disclaimer in the documentation and/or other materials
	provided with the distribution.

   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
   CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
   INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
   DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR
   CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
   NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES ;
   LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
   HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
   CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
   OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE,
   EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.  */

/* Various moxie I/O ports.  */
extern volatile short port_7seg_display;
extern volatile short port_uart[4];
extern volatile short port_pic;

static void fatal_error (short code) __attribute__((noreturn));
static void fatal_error (short code)
{
  port_7seg_display = code;
  while (1);
}

/* Blocking wait for a byte from the serial port.  */
char wait_for_uart_char();

/* A register cache.  */
static int regbuf[20];

static char low_nibble_to_hex (int nibble)
{
  static const char *hex = "0123456789ABCDEF";
  return hex[nibble & 0xf];
}

static char *low_byte_to_hex(int byte)
{
  static char buf[3];
  buf[0] = low_nibble_to_hex(byte >> 4);
  buf[1] = low_nibble_to_hex(byte);
  buf[2] = 0;
  return buf;
}

static char *word2hex(int word)
{
  static char buf[9];
  int i;
  for (i = 0; i < 8; i++)
    buf[i] = low_nibble_to_hex(word >> (28 - i*4));
  buf[8] = 0;
  return buf;
}

/* Global checksum count.  */
static int csum = 0;

static void mx_puts (const char *str, int checksum)
{
  char c;

  while ((c = *str))
    {
      /* Wait for UART to be ready to transmit.  */
      while (port_uart[1] != 1)
	asm ("" : : : "memory");
      port_uart[3] = c;
      if (checksum > 0)
	csum += c;
      str++;
    }
}

static void mx_send_checksum_and_reset ()
{
  mx_puts ("#", 0);
  mx_puts (low_byte_to_hex (csum), 0);
  csum = 0;
}

#define MOXIE_EX_DIV0   0 /* Divide by zero */
#define MOXIE_EX_BAD    1 /* Illegal instruction */
#define MOXIE_EX_IRQ    2 /* Interrupt request */
#define MOXIE_EX_SWI    3 /* Software interrupt */
#define MOXIE_EX_BUSERR 4 /* Bus error exceotion */
#define MOXIE_EX_BRK    5 /* Break instruction  */

void __moxie_exception_handler();

static int hex2int (char c)
{
  switch (c)
    {
    case '0': case '1': case '2': case '3': case '4': 
    case '5': case '6': case '7': case '8': case '9':
      return c - '0';
    case 'A': case 'B': case 'C': 
    case 'D': case 'E': case 'F':
      return c - 'A' + 10;
    case 'a': case 'b': case 'c':
    case 'd': case 'e': case 'f':
      return c - 'a' + 10;
    default:
      return -1;
    }
}

static int read_hex_value_fixed_length (int length)
{
  int n = 0;

  while (length--)
    {
      int c = wait_for_uart_char();
      int v = hex2int(c);
      n = (n << 4) + v;
    }

  return n;
}

static int read_delimited_hex_value ()
{
  int n = 0, v;
  do 
    {
      char c = wait_for_uart_char ();
      v = hex2int(c);
      if (v >= 0)
	n = (n << 4) + v;
    } while (v >= 0);

  return n;
}

void gdb_protocol_handler_loop ()
{
  char c;
  int i;

  /* Main packet waiting loop.  */
  while (1)
    {
      do {
	c = wait_for_uart_char();
      } while (c != '$');

      c = wait_for_uart_char();

      switch (c)
	{
	case 'c':
	  mx_puts ("+", 0);
	  return;
	case 'g':
	  wait_for_uart_char();
	  wait_for_uart_char();
	  wait_for_uart_char();
	  mx_puts ("+$", 0);
	  for (i = 0; i < 18; i++)
	    mx_puts (word2hex (regbuf[i]), 1);
	  mx_send_checksum_and_reset ();
	  break;
	case 'p':
	  {
	    int r = read_delimited_hex_value ();
	    wait_for_uart_char();
	    wait_for_uart_char();
	    mx_puts ("+$", 0);
	    mx_puts (word2hex(regbuf[r]), 1);
	    mx_send_checksum_and_reset ();
	  }
	  break;
	case 'm':
	  {
	    char *addr = (char *) read_delimited_hex_value ();
	    int length = read_delimited_hex_value ();
	    wait_for_uart_char();
	    wait_for_uart_char();
	    mx_puts ("+$", 0);
	    while (length-- > 0)
	      {
		int c = *addr++;
	        mx_puts (low_byte_to_hex(c), 1);
	      }
	    mx_send_checksum_and_reset ();
	  }
	  break;
	case 'M':
	  {
	    char *addr = (char *) read_delimited_hex_value ();
	    int length = read_delimited_hex_value ();
	    while (length-- > 0)
	      {
		int c = read_hex_value_fixed_length (2);
		*addr++ = c;
	      }
	    mx_puts ("+$", 0);
	    mx_puts ("OK", 1);
	    mx_send_checksum_and_reset ();
	  }
	  break;
	case 'P':
	  {
	    int r = read_delimited_hex_value ();
	    int v = read_delimited_hex_value ();
	    regbuf[r] = v;
	    wait_for_uart_char();
	    wait_for_uart_char();
	    mx_puts ("+$", 0);
	    mx_puts ("OK", 1);
	    mx_send_checksum_and_reset ();
	  }
	  break;
	case '?':
	  wait_for_uart_char();
	  wait_for_uart_char();
	  wait_for_uart_char();
	  mx_puts ("+$", 0);
	  mx_puts ("S05", 1);
	  mx_send_checksum_and_reset ();
	  break;
	default:
	  do {
	    c = wait_for_uart_char();
	  } while (c != '#');
	  wait_for_uart_char();
	  wait_for_uart_char();
	  mx_puts ("+$#00", 0);
	}
    }
}

/* Called from our asm code.  Must return the return address.  */
void *__handle_exception (void *faddr, int exc, int code)
{
  switch (exc)
    {
    case MOXIE_EX_DIV0:
    case MOXIE_EX_BAD:
    case MOXIE_EX_SWI:
    case MOXIE_EX_BUSERR:
      fatal_error (0xFF00 & exc);
    case MOXIE_EX_IRQ:
      port_pic = 0;
      return faddr;
    case MOXIE_EX_BRK:
      {
	int i;
	int *fp;
	asm ("mov %0, $fp" : "=r"(fp) : "0"(fp));
	/* Software breakpoint */
	mx_puts ("$", 0);
	mx_puts ("S05", 1);
	mx_send_checksum_and_reset ();
	for (i = 2; i < 16; i++)
	  regbuf[i] = fp[i+1];
	regbuf[0] = fp[0];
	regbuf[1] = (int) fp;
	regbuf[16] = ((int) faddr) - 2;
	gdb_protocol_handler_loop ();
	for (i = 2; i < 16; i++)
	  fp[i+1] = regbuf[i];
	return (void *) regbuf[16];
      }
    default:
      fatal_error (0xFFFF);
    }
  return 0;
}

int main()
{
  int i = 0;

  /* Set the exception handler.  */
  asm("ssr %0, 1" : : "r" (__moxie_exception_handler));

  /* Enable interrupts.  */
  asm("ssr %0, 0" : : "r" (i));

  for (i = 0; i < 20; i++)
    regbuf[i] = 0;
  gdb_protocol_handler_loop ();
  goto *regbuf[16];
}
