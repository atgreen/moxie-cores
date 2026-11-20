# 12345678
	.section .text
	.global	_start
_start:
	/* sto.l with a negative offset */
	ldi.l   $r0, 0x12345678
	ldi.l   $r1, 0x0c0fff00
	sto.l   -32($r1), $r0
	sta.b   0xdead, $r0
