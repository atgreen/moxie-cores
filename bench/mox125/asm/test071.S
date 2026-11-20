# 1234
	.section .text
	.global	_start
_start:	
	/* 0x12345678 / 0x10000 unsigned */
	ldi.l   $r0, 0x12345678
	ldi.l   $r1, 0x10000
	udiv   $r0, $r1
	sta.l   0x0c0ffee0, $r0
	sta.b   0xdead, $r0
