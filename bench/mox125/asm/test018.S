# 56567878
	.section .text
	.global	_start
_start:	
	ldi.l   $sp, 0x00c0ffee4
	ldi.l   $r0, 0x56567878
	nop     # read-after-write hazard
	push    $sp, $r0
	sta.b   0xdead, $r0
