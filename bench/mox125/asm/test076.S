# 12345678
	.section .text
	.global	_start
_start:	
	/* special registers hold what ssr wrote */
	ldi.l $r0, 0x12345678
	ssr $r0, 6
	xor $r0, $r0
	gsr $r1, 6
	sta.l 0x0c0ffee0, $r1
	sta.b 0xdead, $r0
