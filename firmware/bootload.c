#ifndef __MOXIE__
#include <stdio.h>
#include <stdlib.h>
#define wait_for_uart_char getchar
volatile short port_7seg_display;
volatile short port_uart[4];
volatile short port_pic;
#else
char wait_for_uart_char();
extern volatile short port_7seg_display;
extern volatile short port_uart[4];
extern volatile short port_pic;
#endif

#define FATALCODE(code) ((code << 8)+row)

int csum = 0;

static void mx_puts (const char *str)
{
#ifdef __MOXIE__
  while (*str)
    {
#if 1
      /* Wait for UART to be ready to transmit.  */
      while (port_uart[1] != 1)
	{
	  asm ("" : : : "memory");
	}
      port_uart[3] = *str;
      str++;
#else
      port_uart[3] = *str;
      {
	unsigned long c = 500000;
	while (c != 0)
	  {
	    asm ("nop");
	    c--;
	  }
      }
#endif
    }
#else
  printf ("%s", str);
#endif
}

static void fatal_error (short code, const char *msg) // __attribute__((noreturn))
{
  port_7seg_display = code;
  mx_puts ("ERROR: ");
  mx_puts (msg);
  mx_puts ("\n\r");
#ifndef __MOXIE__
  exit (1);
#else
  while (1);
#endif
}

#define MOXIE_EX_DIV0   0 /* Divide by zero */
#define MOXIE_EX_BAD    1 /* Illegal instruction */
#define MOXIE_EX_IRQ    2 /* Interrupt request */
#define MOXIE_EX_SWI    3 /* Software interrupt */
#define MOXIE_EX_BUSERR 4 /* Software interrupt */

/* Called from our asm code.  Must return the return address.  */
void *__handle_exception (void *faddr, int exc, int code)
{
  static int c = 0x0;
  static int q = 0;

  switch (exc)
    {
    case MOXIE_EX_DIV0:
      fatal_error (0, "DIVIDE BY ZERO EXCEPTION");
    case MOXIE_EX_BAD:
      fatal_error (0, "ILLEGAL INSTRUCTION EXCEPTION");
    case MOXIE_EX_IRQ:
      q++;
      if (q == 4) 
	{
	  q = 0;
	  /*	  port_7seg_display = c++; */
	}
      // Clear the timer interrupt.
      port_pic = 0;
      return faddr;
    case MOXIE_EX_SWI:
      fatal_error (0, "SOFTWARE INTERRUPT REQUEST");
    case MOXIE_EX_BUSERR:
      fatal_error (0, "BUS ERROR");
      // TRY LOOPING FOREVER
      while (1);
    default:
      fatal_error (0, "UNKNOWN EXCEPTION");
    }
  return 0;
}

void __moxie_exception_handler();

static int hex2int (char c)
{
  int n;

  switch (c)
    {
    case '0':
    case '1':
    case '2':
    case '3':
    case '4':
    case '5':
    case '6':
    case '7':
    case '8':
    case '9':
      return c - '0';
    case 'A':
    case 'B':
    case 'C':
    case 'D':
    case 'E':
    case 'F':
      return c - 'A' + 10;
    case 'a':
    case 'b':
    case 'c':
    case 'd':
    case 'e':
    case 'f':
      return c - 'a' + 10;
    default:
      fatal_error (0, "Illegal S-Record row length value");
    }
}

static int read_hex_value (int length)
{
  int n = 0;

  while (length--)
    {
      int v = hex2int(wait_for_uart_char());
      csum += v;
      n = (n << 4) + v;
    }

  return n;
}

static int record_type_address_length (int record_type)
{
  switch (record_type)
    {
    case '0':
      return 2;
    case '1':
      return 2;
    case '2':
      return 3;
    case '3':
      return 4;
    case '5':
      return 2;
    case '7':
      return 4;
    case '8':
      return 3;
    case '9':
      return 2;
    default:
      fatal_error (record_type, "Illegal S-Record record type");
    }
}

#ifdef __MOXIE__
#define STORE(T,A,V) *(T*)(A) = V
#else
#define STORE(T,A,V)
#endif

char nibble2hex(int nibble)
{
  if (nibble < 10)
    return '0' + nibble;
  else
    return 'A' + nibble - 10;
}

char *byte2hex(int byte)
{
  static char buf[8];
  buf[0] = '[';
  buf[1] = nibble2hex(byte >> 4);
  buf[2] = nibble2hex(byte & 0xF);
  buf[3] = ']';
  buf[4] = 0;
  return buf;
}

int main()
{
  int i = 0, done = 0;
  short row = 1;
  short c, record_type, length;
  char *address;

#ifdef __MOXIE__  
  /* Set the exception handler.  */
  asm("ssr %0, 1" : : "r" (__moxie_exception_handler));

  /* Enable interrupts.  */
  asm("ssr %0, 0" : : "r" (i));
#endif

  /* Print out welcome message.  */
  mx_puts ("MOXIE On-Chip Bootloader v1.0\n\r");
  mx_puts ("Copyright (c) 2013 Anthony Green <green@moxielogic.com>\n\r");
  mx_puts ("\n\r");
  mx_puts ("Waiting for S-Record Download...\n\r");

  while (! done)
    {
      int csum = 0;

      /* Get the start of the s-record.  */
      do {
	c = wait_for_uart_char();
      } while (c != 'S');

      /* Get the record type.  */
      record_type = wait_for_uart_char();

      if ((record_type < '0') | (record_type > '9'))
	fatal_error (FATALCODE(record_type), "Illegal S-Record record type");

      /* Get the record length in 2-char bytes.  */
      length = read_hex_value(2);

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
	    address = (char *) read_hex_value (8);
	    length -= 4;

	    while (length > 4)
	      {
		int value = read_hex_value (8);
		// port_7seg_display = ((int)address) >> 16;
		STORE(int,address,value);
		address += 4;
		length -= 4;
	      }

	    while (length > 2)
	      {
		short value = read_hex_value (4);
		// port_7seg_display = ((int)address) >> 16;
		STORE(short,address,value);
		address += 2;
		length -= 2;
	      }

	    while (length > 1)
	      {
		char value = read_hex_value (2);
		// port_7seg_display = ((int)address) >> 16;
		STORE(char,address,value);
		address += 1;
		length -= 1;
	      }

	    /* Skip checksum for now */
	    read_hex_value (2);
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
	  fatal_error (FATALCODE(record_type), "Unhandled S-Record record type");
	  break;
	}
      row++;
    }

  port_7seg_display = 0xf00d;

  mx_puts ("Jumping to code at 0x30000000.\n\r");

#ifdef __MOXIE__
  /* Jump to our new program in RAM.  Never return.  */
  asm ("jmpa 0x30000000");
#endif

  return 0;
}
