# c3a57e8e
	/* Bytes in all three addressing modes, at even and odd
	   addresses.  The result is built a byte at a time, and its
	   last byte is zero if a load failed to zero-extend.  */
	.section .text
	.global	_start
_start:
	ldi.l	$r0, 0x123456c3
	sta.b	0x0c0ffee0, $r0
	ldi.l	$r1, 0x0c0ffee0
	ldi.l	$r2, 0xa5
	sto.b	1($r1), $r2
	ldi.l	$r3, 0x0c0ffee2
	ldi.l	$r4, 0x7e
	st.b	($r3), $r4

	ldi.l	$r5, 0x99
	sta.b	0x2001, $r5
	ldi.l	$r6, 0x2000
	ldo.b	$r7, 1($r6)
	lda.b	$r8, 0x2001
	add	$r7, $r8
	ldi.l	$r9, 0x2001
	ld.b	$r10, ($r9)
	add	$r7, $r10
	sta.b	0x2002, $r0
	lda.b	$r11, 0x2002
	add	$r7, $r11
	ldi.l	$r12, 0x28e
	cmp	$r7, $r12
	beq	1f
	ldi.l	$r7, 0
1:	sto.b	3($r1), $r7
	sta.b	0xdead, $r7
