# F8000000
	.section .text
	.global	_start
_start:	
	/* arithmetic shift keeps the sign */
	ldi.l $r0, 0x80000000
	ldi.l $r1, 4
	ashr $r0, $r1
	sta.l 0x0c0ffee0, $r0
	sta.b 0xdead, $r0
