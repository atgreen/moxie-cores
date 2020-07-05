# 3
	.section .text
	.global	_start
_start:	
	/* End simulation by writing to 0x00c0ffee */
	xor     $r5, $r5
	ldi.l	$r8, 3
_here:	inc	$r5, 1
	cmp	$r5, $r8
	bne	_here
	sta.l   0x0c0ffee0, $r5
	sta.b   0xdead, $r5

