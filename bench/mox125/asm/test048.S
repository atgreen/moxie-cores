# 12345678
	.section .text
	.global	_start
_start:	
	/* push then pop */
	ldi.l $sp, 0x0c0ffd00
	ldi.l $r0, 0x12345678
	push $sp, $r0
	pop $sp, $r1
	sta.l 0x0c0ffee0, $r1
	sta.b 0xdead, $r0
