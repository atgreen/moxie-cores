# 2A
	.section .text
	.global	_start
_start:	
	ldi.l $r0, 6
	ldi.l $r1, 7
	mul $r0, $r1
	sta.l 0x0c0ffee0, $r0
	sta.b 0xdead, $r0
