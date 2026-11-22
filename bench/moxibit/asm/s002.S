# 37
	/* Sum 1..10 with a backward branch.  */
	.section .text
	.global	_start
_start:
	ldi.l	$r0, 0
	ldi.l	$r1, 0
	ldi.l	$r2, 1
	ldi.l	$r3, 10
1:	add	$r1, $r2
	add	$r0, $r1
	cmp	$r1, $r3
	bne	1b
	sta.l	0x0c0ffee0, $r0
	sta.l	0xdead, $r0
