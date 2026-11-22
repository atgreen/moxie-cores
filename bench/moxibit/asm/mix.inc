	/* Fold \r into the checksum in $r13, using $r11 and the shift
	   count 7 in $r12.  */
	.macro	mix r
	add	$r13, \r
	mov	$r11, $r13
	ashl	$r11, $r12
	xor	$r13, $r11
	.endm

	.macro	mixinit
	ldi.l	$r13, 0x5eed
	ldi.l	$r12, 7
	.endm

	.macro	finish
	sta.l	0x0c0ffee0, $r13
	sta.b	0xdead, $r13
	.endm
