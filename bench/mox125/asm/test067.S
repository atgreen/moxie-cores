# 2
	.section .text
	.global	_start
_start:	
	/* 100 % -7 */
	ldi.l   $r0, 0x64
	ldi.l   $r1, 0xfffffff9
	mod    $r0, $r1
	sta.l   0x0c0ffee0, $r0
	sta.b   0xdead, $r0
