# 12345678
	.section .text
	.global	_start
_start:	
	ldi.l $r0, 0x12345678
	sta.l 0x0c0ffd00, $r0
	lda.l $r1, 0x0c0ffd00
	sta.l 0x0c0ffee0, $r1
	sta.b 0xdead, $r0
