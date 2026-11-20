# 1C
	.section .text
	.global	_start
_start:	
	/* quotient used by the next instruction */
	ldi.l   $r0, 0x64
	ldi.l   $r1, 0x7
	div    $r0, $r1
	add     $r0, $r0
	sta.l   0x0c0ffee0, $r0
	sta.b   0xdead, $r0
