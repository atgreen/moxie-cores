# 0
	.section .text
	.global	_start
_start:	
	/* 7 / 100 */
	ldi.l   $r0, 0x7
	ldi.l   $r1, 0x64
	div    $r0, $r1
	sta.l   0x0c0ffee0, $r0
	sta.b   0xdead, $r0
