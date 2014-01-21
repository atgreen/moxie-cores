/* xdl.c - a simple xmodem-crc srecord receive implementation

   Copyright (c) 2014, Anthony Green

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

#define TIMEOUT 3000000
#define FATALCODE(code) ((code << 8)+row)
#define STORE(T,A,V) *(T*)(A) = V

static void fatal_error (short code) __attribute__((noreturn));
static void fatal_error (short code)
{
  port_7seg_display = code;
  while (1);
}

static void moxie_putchar(char c)
{
  while (port_uart[1] != 1)
    asm ("" : : : "memory");
  port_uart[3] = c;
}

static void moxie_puts (const char *str)
{
  char c;

  while ((c = *str))
    {
      moxie_putchar(c);
      str++;
    }
}

static int moxie_readchar (unsigned long delay)
{
  while (port_uart[0] == 0)
    {
      if (delay-- == 0)
	{
	  port_7seg_display = 0x9999;
	  return -1;
	}
    }
  return port_uart[2] & 0xff;
}

void moxie_flush_input ()
{
  short junk = 0;
  while (port_uart[0])
    junk += port_uart[2];
}

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

static int moxie_rxready()
{
  return port_uart[0];
}

/* The packet is made up of..
   1 byte header
   2 byte packet number encoding
   128 bytes of data
   2 bytes of checksum.  */
#define XMODEM_PACKET_SIZE (1 + 2 + 128 + 2)

#define XMODEM_SOH  0x01
#define XMODEM_EOT  0x04
#define XMODEM_ACK  0x06
#define XMODEM_NAK  0x15

/* Return codes from our packet receive function.  */
#define RX_OK                        0
#define RX_TIMEOUT                   1
#define RX_EOT                       2
#define RX_BAD_PACKET_NUMBER         3
#define RX_DUPLICATE_PACKET_NUMBER   4
#define RX_UNEXPECTED_PACKET_NUMBER  5
#define RX_BAD_CHECKSUM              6
#define RX_BAD_HEADER                7

typedef struct 
{
  unsigned char expected_packet_number;
  char ack;
  int read_index;
  int finished;
  unsigned char buffer[XMODEM_PACKET_SIZE];
} rx_state_t;

static void init_rx_state (rx_state_t *rxs)
{
  rxs->finished = 0;
  rxs->expected_packet_number = 1;
  rxs->ack = 0;
  rxs->read_index = 128+3;
}

static int xmodem_rx_packet (rx_state_t *rxs)
{
  int c, i = 0;
  short crc = 0;
  unsigned char *buffer = rxs->buffer;

  do 
    {
      c = moxie_readchar (TIMEOUT);
      if (c == -1)
	{
	  if (buffer[0] == XMODEM_EOT)
	    return RX_EOT;
	  else
	    return RX_TIMEOUT;
	}
      else
	buffer[i++] = c;
    } while (i < XMODEM_PACKET_SIZE);

  port_7seg_display = 0x4448;

  if (buffer[0] == XMODEM_EOT)
    {
      return RX_EOT;
    }

  if (buffer[0] != XMODEM_SOH)
    {
      return RX_BAD_HEADER;
    }

  if (buffer[1] != (~buffer[2] & 0xff))
    return RX_BAD_PACKET_NUMBER;

  if (buffer[1] == (rxs->expected_packet_number - 1))
    {
      return RX_DUPLICATE_PACKET_NUMBER;
    }

  if (buffer[1] != rxs->expected_packet_number)
    {
      return RX_UNEXPECTED_PACKET_NUMBER;
    }

  /* Now we check the buffer checksum.  */
  for (i = 3; i < XMODEM_PACKET_SIZE-2; i++)
    {
      int r;
      crc = crc ^ (((short)buffer[i]) << 8);
      
      for (r = 0; r < 8; r++)
	{
	  if ((crc & 0x8000) != 0)
	    crc = (crc << 1) ^ 0x1021;
	  else
	    crc = (crc << 1);
	}
    }

  if ((buffer [XMODEM_PACKET_SIZE - 2] != ((crc >> 8) & 0xff))
      || (buffer [XMODEM_PACKET_SIZE - 1] != (crc & 0xff)))
    return RX_BAD_CHECKSUM;

  rxs->expected_packet_number++;
  return RX_OK;
}

static int xmodem_read_char (rx_state_t *rxs)
{
  if (rxs->finished)
    return -1;
  else if (rxs->read_index == 128+3)
    {
      int rc;
      
      if (rxs->ack)
	{
	  moxie_putchar (XMODEM_ACK);
	  rxs->ack = 0;
	}
      for (;;)
	switch (rc = xmodem_rx_packet (rxs))
	  {
	  case RX_OK:
	    rxs->ack = XMODEM_ACK;
	    rxs->read_index = 4;
	    return rxs->buffer[3];
	    
	  case RX_EOT:
	    moxie_putchar (XMODEM_ACK);
	    rxs->finished = 1;
	    return -1;
	    
	  default:
	    port_7seg_display = 0x3300 + (rc & 0xff);
	    moxie_flush_input ();
	    moxie_putchar (XMODEM_NAK);
	    break;
	  }
    }
  else
    return rxs->buffer[rxs->read_index++];
}

static int read_hex_value_fixed_length (rx_state_t *rxs, int length)
{
  int n = 0;

  while (length--)
    {
      int v;
      int c = xmodem_read_char (rxs);
      if (c == -1)
	fatal_error(7777);
      v = hex2int(c);
      n = (n << 4) + v;
    }

  return n;
}

int main ()
{
  int done = 0;
  short row = 1;
  short c, record_type, length;
  char *address;
  rx_state_t rxs;

  port_7seg_display = 0x4444;
  init_rx_state (&rxs);

  moxie_flush_input ();

  /* Start the transfer with 'C', indicating an xmodem-crc download.  */
  do
    {
      unsigned long timeout = 3000000;
      moxie_putchar ('C');
      while (timeout-- && !moxie_rxready())
	asm ("");
    } while (! moxie_rxready ());

  port_7seg_display = 0x4446;

  while (! done)
    {
      /* Get the start of the s-record.  */
      do {
	c = xmodem_read_char(&rxs);
      } while (c != 'S');

      /* Get the record type.  */
      record_type = xmodem_read_char(&rxs);

      if ((record_type < '0') | (record_type > '9'))
	fatal_error (FATALCODE(record_type));

      /* Get the record length in 2-char bytes.  */
      length = read_hex_value_fixed_length (&rxs, 2);

      port_7seg_display = row;

      switch (record_type)
	{
	case '0':
	  length = length*2;
	  while (length--)
	    xmodem_read_char (&rxs);
	  break;
	case '3':
	  {
	    /* Get the record address.  */
	    address = (char *) read_hex_value_fixed_length (&rxs, 8);
	    length -= 4;

	    while (length > 4)
	      {
		int value = read_hex_value_fixed_length (&rxs, 8);
		STORE(int,address,value);
		address += 4;
		length -= 4;
	      }

	    while (length > 2)
	      {
		short value = read_hex_value_fixed_length (&rxs, 4);
		STORE(short,address,value);
		address += 2;
		length -= 2;
	      }

	    while (length > 1)
	      {
		char value = read_hex_value_fixed_length (&rxs, 2);
		STORE(char,address,value);
		address += 1;
		length -= 1;
	      }

	    /* Skip checksum for now */
	    read_hex_value_fixed_length (&rxs, 2);
	  }
	  break;
	case '7':
	  done = 1;
	  length = length*2;
	  while (length--)
	    xmodem_read_char (&rxs);
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

  while (xmodem_read_char (&rxs) != -1);

  moxie_puts ("Jumping to code at 0x30000000.\n\r");

  /* Hint that we're just about to jump to 0x30000000.  */
  port_7seg_display = 0x3000;

  /* Jump to our new program in RAM.  Never return.  */
  asm ("jmpa 0x30000000");

  port_7seg_display = 0x5555;
  return 0;
}

