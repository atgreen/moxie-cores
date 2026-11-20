# 22222222
	.section .text
	.global	_start
_start:	
	/* load-use hazard */
	ldi.l $r0, 0x11111111
	sta.l 0x0c0ffd00, $r0
	lda.l $r1, 0x0c0ffd00
	add $r1, $r1
	sta.l 0x0c0ffee0, $r1
	sta.b 0xdead, $r0
