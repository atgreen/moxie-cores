# 3
	.section .text
	.global	_start
_start:	
	/* End simulation by writing to 0x00c0ffee */
	ldi.l	$r8, 3
	ldi.l	$r9, 0x0c0ffe00
	sto.l   0xe0($r9), $r8
	sta.b   0xdead, $r8

