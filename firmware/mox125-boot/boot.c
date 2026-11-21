/* boot.c - a boot loader for mox125 on a board with real RAM.

   Copyright (c) 2026  Anthony Green

   The above named program is free software; you can redistribute it
   and/or modify it under the terms of the GNU General Public License
   version 2 as published by the Free Software Foundation.

   The above named program is distributed in the hope that it will be
   useful, but WITHOUT ANY WARRANTY; without even the implied warranty
   of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this work; if not, write to the Free Software
   Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA
   02110-1301, USA.  */

/* This lives in a ROM out of the way at the top of the address space,
   on a board whose peripherals are those of the mox125_sim board (see
   cores/mox125_board) and whose RAM starts at address zero.  Programs
   built for the simulators are linked to run at 0x1000, which is
   where this starts them.

   It checks the RAM, then takes commands from the serial port:

     S-records   load a program; the S7, S8 or S9 record that ends it
                 starts it at the address it gives
     g           start whatever is at 0x1000, which after a reset is
                 the last program loaded, RAM being what it is
     t           check the RAM again

   Make S-records with
     moxie-elf-objcopy -O srec --srec-forceS3 program.elf program.srec
   and send the file as it is.  Each record is answered with a '.', or
   a '!' if it arrived damaged.  */

#define CONSOLE		(*(volatile unsigned char *) 0xF0000000)
#define DISPLAY		(*(volatile unsigned short *) 0xF0000040)
#define UART_RBR	(*(volatile unsigned char *) 0xF0000100)
#define UART_LSR	(*(volatile unsigned char *) 0xF0000114)

#define RAM_BYTES	(16 << 20)
#define START_ADDRESS	0x1000

extern void start_program (unsigned long address) __attribute__ ((noreturn));

static void
put_char (char c)
{
  /* The write waits until there is room for it.  */
  CONSOLE = c;
}

static void
put_string (const char *s)
{
  while (*s)
    {
      if (*s == '\n')
	put_char ('\r');
      put_char (*s++);
    }
}

static void
put_hex (unsigned long v)
{
  int i;

  for (i = 28; i >= 0; i -= 4)
    put_char ("0123456789abcdef"[(v >> i) & 15]);
}

static int
get_char (void)
{
  while ((UART_LSR & 1) == 0)
    ;
  return UART_RBR;
}

/* Each power-of-two address gets a word saying which it is, so that an
   address line that is stuck, or shorted to its neighbour, shows up as
   two addresses holding the same thing.  Then bytes, since they take a
   different path to the memory than words do.  Everything is put back
   afterwards: there may be a program in there that "g" is about to
   start.  */

#define TEST_WORDS	23	/* address 0, and 4 up to 8 MiB */
#define BYTE_TEST_AT	0x100

static volatile unsigned long *
test_word (int i)
{
  return (volatile unsigned long *) (i == 0 ? 0 : 2ul << i);
}

static int
ram_ok (void)
{
  volatile unsigned char *byte = (volatile unsigned char *) BYTE_TEST_AT;
  unsigned long saved[TEST_WORDS], saved_bytes, got = 0;
  int i, bad = -1, bytes_ok;

  for (i = 0; i < TEST_WORDS; i++)
    saved[i] = *test_word (i);
  for (i = 0; i < TEST_WORDS; i++)
    *test_word (i) = (unsigned long) test_word (i) ^ 0xa5a55a5a;
  for (i = TEST_WORDS - 1; i >= 0; i--)
    {
      if (*test_word (i) != ((unsigned long) test_word (i) ^ 0xa5a55a5a))
	{
	  bad = i;
	  got = *test_word (i);
	}
      *test_word (i) = saved[i];
    }

  if (bad >= 0)
    {
      put_string ("RAM: bad word at ");
      put_hex ((unsigned long) test_word (bad));
      put_string (", read ");
      put_hex (got);
      put_string ("\n");
      return 0;
    }

  saved_bytes = *(volatile unsigned long *) byte;
  *(volatile unsigned long *) byte = 0;
  for (i = 0; i < 4; i++)
    byte[i] = 0x11 * (i + 1);
  got = *(volatile unsigned long *) byte;
  bytes_ok = (got == 0x11223344 && byte[0] == 0x11 && byte[1] == 0x22
	      && byte[2] == 0x33 && byte[3] == 0x44);
  *(volatile unsigned long *) byte = saved_bytes;

  if (!bytes_ok)
    {
      put_string ("RAM: bytes are in the wrong place, read ");
      put_hex (got);
      put_string ("\n");
      return 0;
    }

  put_string ("RAM: 16 MB ok\n");
  return 1;
}

static int
hex_digit (int c)
{
  if (c >= '0' && c <= '9')
    return c - '0';
  c |= 0x20;
  if (c >= 'a' && c <= 'f')
    return c - 'a' + 10;
  return -1;
}

/* Two digits from the serial port, or -1.  */

static int
get_hex_byte (void)
{
  int hi = hex_digit (get_char ());
  int lo;

  if (hi < 0)
    return -1;
  lo = hex_digit (get_char ());
  if (lo < 0)
    return -1;
  return (hi << 4) | lo;
}

/* The 'S' has been read.  Take in the rest of the record.  Returns the
   address to start at if it was the last one, 1 if it was not, and 0
   if it was no good.  Nothing is ever loaded at 1.  */

static unsigned long
get_record (void)
{
  int type = get_char ();
  int count = get_hex_byte ();
  int address_bytes, sum, b, i;
  unsigned long address = 0;
  int is_data = 0, is_end = 0;

  switch (type)
    {
    case '1': address_bytes = 2; is_data = 1; break;
    case '2': address_bytes = 3; is_data = 1; break;
    case '3': address_bytes = 4; is_data = 1; break;
    case '7': address_bytes = 4; is_end = 1; break;
    case '8': address_bytes = 3; is_end = 1; break;
    case '9': address_bytes = 2; is_end = 1; break;
    default:  address_bytes = 0; break;	/* S0, S5: nothing we want */
    }

  if (count < address_bytes + 1)
    return 0;
  sum = count;

  for (i = 0; i < address_bytes; i++)
    {
      b = get_hex_byte ();
      if (b < 0)
	return 0;
      sum += b;
      address = (address << 8) | b;
    }

  for (i = count - address_bytes - 1; i > 0; i--)
    {
      b = get_hex_byte ();
      if (b < 0)
	return 0;
      sum += b;
      if (is_data)
	*(volatile unsigned char *) address++ = b;
    }

  b = get_hex_byte ();
  if (b < 0 || ((sum + b) & 0xff) != 0xff)
    return 0;

  return is_end ? address : 1;
}

int
main (void)
{
  unsigned long result;
  unsigned records = 0;
  int bad = 0;

  DISPLAY = 0xb007;
  put_string ("\nmox125 boot loader\n");
  ram_ok ();
  put_string ("Send S-records, or g to run what is at 0x1000.\n");

  for (;;)
    switch (get_char ())
      {
      case 'S':
	result = get_record ();
	if (result == 0)
	  {
	    bad = 1;
	    put_char ('!');
	  }
	else if (result == 1)
	  {
	    /* Sixteen bits of how far along we are.  */
	    DISPLAY = ++records;
	    put_char ('.');
	  }
	else if (bad)
	  {
	    put_string ("\nNot starting: some of that did not arrive.\n");
	    bad = 0;
	    records = 0;
	  }
	else
	  {
	    put_string ("\nStarting at ");
	    put_hex (result);
	    put_string ("\n");
	    start_program (result);
	  }
	break;

      case 'g':
	put_string ("Starting at 00001000\n");
	start_program (START_ADDRESS);

      case 't':
	ram_ok ();
	break;

      default:
	break;
      }
}
