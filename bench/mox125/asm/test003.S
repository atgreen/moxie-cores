# 6789
	.section .text
	.global	_start
_start:	
	ldi.s   $r0, 0x6789
	sta.l   0x0c0ffee0, $r0
	sta.b   0xdead, $r0

