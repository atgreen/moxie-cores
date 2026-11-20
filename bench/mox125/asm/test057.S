# 1
	.section .text
	.global	_start
_start:	
	/* jump through a register */
	xor $r5, $r5
	ldi.l $r1, _here
	jmp $r1
	ldi.l $r5, 0xbad
_here:
	inc $r5, 1
	sta.l 0x0c0ffee0, $r5
	sta.b 0xdead, $r5
