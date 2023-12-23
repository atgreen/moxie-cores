# 0
	.section .text
	.global	_start
_start:
	/* End simulation by writing to 0xdead */
	xor   $r5, $r5
	ldi.l	$r8, 5
	ldi.l	$r9, 5
	cmp	  $r8, $r9
	beq   _here
  ldi.l $r5, 7
_here:
  sta.l 0x0c0ffee0, $r5
	sta.b 0xdead, $r5
