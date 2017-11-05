# 12345678
	.section .text
	.global	_start
_start:	
	ldi.l   $r0, 0x12345678
	ldi.l   $r1, 0x0c0ffe00
	sto.l   0xe0($r1), $r0
	sta.b   0xdead, $r0
