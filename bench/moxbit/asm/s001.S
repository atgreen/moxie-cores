# 80000001
	/* The carry has to ripple all the way up, and adding a negative
	   number has to wrap.  */
	.section .text
	.global	_start
_start:
	ldi.l	$r0, 0x7fffffff
	ldi.l	$r1, 0x00000003
	add	$r0, $r1
	ldi.l	$r2, 0xffffffff
	add	$r0, $r2
	nop
	sta.l	0x0c0ffee0, $r0
	sta.l	0xdead, $r0
