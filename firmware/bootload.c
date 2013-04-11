extern volatile short port_7seg_display;
extern volatile short port_uart;
extern volatile short port_pic;

static void mx_puts (const char *str)
{
  while (*str)
    {
      port_uart = (short)*str;
      str++;
    }
}

static char wait_for_uart_char()
{
  short c;
  do
    {
      c = port_uart;
    } while (c > 0xff);
  return (char) c;
}

static void fatal_error (const char *msg) // __attribute__((noreturn))
{
  mx_puts ("ERROR: ");
  mx_puts (msg);
  while (1);
}

#define MOXIE_EX_DIV0 0 /* Divide by zero */
#define MOXIE_EX_BAD  1 /* Illegal instruction */
#define MOXIE_EX_IRQ  2 /* Interrupt request */
#define MOXIE_EX_SWI  3 /* Software interrupt */

/* Called from our asm code.  Must return the return address.  */
void *__handle_exception (void *faddr, int exc, int code)
{
  static int c = 0x0;
  static int q = 0;

  switch (exc)
    {
    case MOXIE_EX_DIV0:
      fatal_error ("DIVIDE BY ZERO EXCEPTION");
    case MOXIE_EX_BAD:
      fatal_error ("ILLEGAL INSTRUCTION EXCEPTION");
    case MOXIE_EX_IRQ:
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
      fatal_error ("SOFTWARE INTERRUPT REQUEST");
    default:
      fatal_error ("UNKNOWN EXCEPTION");
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
      fatal_error ("Illegal S-Record row length value");
    }
}

static int read_hex_value (int length)
{
  int n = 0;

  while (length--)
    {
      n = (n << 8) + hex2int(wait_for_uart_char());
    }

  return n;
}

static int record_type_address_length (int record_type)
{
  switch (record_type)
    {
    case '0':
      return 4;
    case '1':
      return 4;
    case '2':
      return 6;
    case '3':
      return 8;
    case '5':
      return 4;
    case '7':
      return 8;
    case '8':
      return 6;
    case '9':
      return 4;
    default:
      fatal_error ("Illegal S-Record record type");
    }
}

int main()
{
  int i = 0;
  char buf[256];
  short c, record_type, length;
  char *address;
  
  /* Set the exception handler.  */
  asm("ssr %0, 1" : : "r" (__moxie_exception_handler));

  /* Enable interrupts.  */
  asm("ssr %0, 0" : : "r" (i));

  /* Print out welcome message.  */
  mx_puts ("MOXIE On-Chip Bootloader v1.0\n\r");
  mx_puts ("  Copyright (c) 2013 Anthony Green <green@moxielogic.com>\n\r");
  mx_puts ("\n\r");
  mx_puts ("Waiting for S-Record Download...\n\r");

  while (! done)
    {
      /* Get the start of the s-record.  */
      do {
	c = wait_for_uart_char();
      } while (c != 'S');

      /* Get the record type.  */
      record_type = wait_for_uart_char();
      if (record_type < '0' || record_type > '9')
	fatal_error ("Illegal S-Record record type");

      /* Get the record length.  */
      length = read_hex_value(2) * 2;

      if (record_type == '3')
	{
	  /* Get the record address.  */
	  address = (char *) read_hex_value (record_type_address_length (record_type));
	  length -= record_type_address_length (record_type);

	  while (length < 5)
	    {
	      int value = read_hex_value (8);
	      *(int *)address = value;
	      address += 4;
	      length -= 4;
	    }

	  if (length < 3)
	    {
	      short value = read_hex_value (2);
	      *(short *)address = value;
	      address += 2;
	      length -= 2;
	    }

	  if (length < 2)
	    {
	      char value = read_hex_value (1);
	      *(char *)address = value;
	      address += 1;
	      length -= 1;
	    }

	  /* Skip checksum for now */
	  read_hex_value (2);
	}
      else if (record_type == '9')
	done = true;
    }

  /* Jump to our new program in RAM.  Never return.  */
  asm ("jmpa 0x3000000");

  return 0;
}
