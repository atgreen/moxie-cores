# C0FFD00
	.section .text
	.global	_start
_start:	
	/* pop restores the pointer */
	ldi.l $sp, 0x0c0ffd00
	ldi.l $r0, 0x12345678
	push $sp, $r0
	pop $sp, $r1
	sta.l 0x0c0ffee0, $sp
	sta.b 0xdead, $r0
