# 0
	.section .text
	.global	_start
_start:	
	/* 0x5 / 0x80000000 unsigned */
	ldi.l   $r0, 0x5
	ldi.l   $r1, 0x80000000
	udiv   $r0, $r1
	sta.l   0x0c0ffee0, $r0
	sta.b   0xdead, $r0
