# 1
	.section .text
	.global	_start
_start:
	/* End simulation by writing to 0xdead */
	xor   $r5, $r5
	ldi.l $r8, 3
  nop
  inc	  $r5, 1
	sta.l 0x0c0ffee0, $r5
	sta.b 0xdead, $r5
