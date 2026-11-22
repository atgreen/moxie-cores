# 22446688
	/* Longs in all three addressing modes, at addresses that are
	   only halfword aligned, with a negative offset.  */
	.section .text
	.global	_start
_start:
	ldi.l	$r0, 0x11223344
	ldi.l	$r1, 0x2002
	st.l	($r1), $r0
	ld.l	$r2, ($r1)
	sto.l	6($r1), $r2
	ldi.l	$r3, 0x2010
	ldo.l	$r4, -8($r3)
	sta.l	0x2022, $r4
	lda.l	$r5, 0x2022
	add	$r5, $r4
	sta.l	0x0c0ffee0, $r5
	sta.b	0xdead, $r5
