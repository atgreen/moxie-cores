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

/* Are we reading the first character?  */
static short first;

/* Global checksum count.  */
static int csum = 0;

/* Blocking wait for a byte from the serial port.  */
char wait_for_uart_char();

/* The exception handling routine.  */
void __moxie_exception_handler();

static void fatal_error (short code) __attribute__((noreturn));
static void fatal_error (short code)
{
  port_7seg_display = code;
  while (1);
}

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

#define FATALCODE(code) ((code << 8)+row)
#define STORE(T,A,V) *(T*)(A) = V

void download_srecord_executable ()
{
  int done = 0;
  short row = 1;
  short c, record_type, length;
  char *address;

  while (! done)
    {
      /* Get the start of the s-record.  */
      if (! first)
	{
	  do {
	    c = wait_for_uart_char();
	  } while (c != 'S');
	}
      else
	first = 0;

      /* Get the record type.  */
      record_type = wait_for_uart_char();

      if ((record_type < '0') | (record_type > '9'))
	fatal_error (FATALCODE(record_type));

      /* Get the record length in 2-char bytes.  */
      length = read_hex_value_fixed_length (2);

      port_7seg_display = row;

      switch (record_type)
	{
	case '0':
	  length = length*2;
	  while (length--)
	    wait_for_uart_char ();
	  break;
	case '3':
	  {
	    /* Get the record address.  */
	    address = (char *) read_hex_value_fixed_length (8);
	    length -= 4;

	    while (length > 4)
	      {
		int value = read_hex_value_fixed_length (8);
		STORE(int,address,value);
		address += 4;
		length -= 4;
	      }

	    while (length > 2)
	      {
		short value = read_hex_value_fixed_length (4);
		STORE(short,address,value);
		address += 2;
		length -= 2;
	      }

	    while (length > 1)
	      {
		char value = read_hex_value_fixed_length (2);
		STORE(char,address,value);
		address += 1;
		length -= 1;
	      }

	    /* Skip checksum for now */
	    read_hex_value_fixed_length (2);
	  }
	  break;
	case '7':
	  done = 1;
	  length = length*2;
	  while (length--)
	    wait_for_uart_char ();
	  break;
	case '9':
	  done = 1;
	  break;
	default:
	  fatal_error (FATALCODE(record_type));
	  break;
	}
      row++;
    }

  mx_puts ("Jumping to code at 0x30000000.\n\r", 0);

  /* Hint that we're just about to jump to 0x30000000.  */
  port_7seg_display = 0x3000;

  /* Jump to our new program in RAM.  Never return.  */
  asm ("jmpa 0x30000000");
}

void gdb_protocol_handler_loop ()
{
  char c;
  int i;

  /* Main packet waiting loop.  */
  while (1)
    {

      if (! first)
	{
	  do {
	    c = wait_for_uart_char();
	  } while (c != '$');
	}
      else
	first = 0;

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
	  break;
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
	regbuf[1] = fp[0];
	regbuf[0] = *(int*)fp[0];
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
  char c;

  /* Set the exception handler.  */
  asm("ssr %0, 1" : : "r" (__moxie_exception_handler));

  /* Enable interrupts.  */
  asm("ssr %0, 0" : : "r" (i));

  /* Reset the global checksum.  */
  csum = 0;

  /* Print out welcome message.  */
  mx_puts ("MOXIE On-Chip Bootloader v2.0\n\r", 0);
  mx_puts ("Copyright (c) 2013 Anthony Green <green@moxielogic.com>\n\r", 0);
  mx_puts ("\n\r", 0);
  mx_puts ("Waiting for an S-Record Download or Remote GDB Connection...\n\r", 0);

  /* Update global state.  We've just read our first character.  */
  first = 1;

  /* Wait for either an S-Record download, or a remote gdb connection.  */
  do {
    c = wait_for_uart_char();
  } while ((c != '$') && (c != 'S'));

  if (c == '$')
    {
      /* We're connecting to gdb... */
      int i;

      /* Indicate that we're in remote debug mode.  */
      port_7seg_display = 0xdeb2;

      /* Clear register cache, and start debugging.  */
      for (i = 0; i < 20; i++)
	regbuf[i] = 0;
      gdb_protocol_handler_loop ();
      goto *regbuf[16];
    }
  else
    {
      /* We're doing an srecord file download... */
      download_srecord_executable ();
    }

  while (1);
}
