# 80008003
	/* Shorts in all three addressing modes.  Loads zero-extend:
	   0xffff8001 instead of 0x00008001 would make 0x7ffe8001.  */
	.section .text
	.global	_start
_start:
	ldi.l	$r0, 0x12348001
	ldi.l	$r1, 0x2000
	st.s	($r1), $r0
	ld.s	$r2, ($r1)
	ldi.l	$r3, 0x7fff0000
	add	$r2, $r3
	sto.s	4($r1), $r0
	ldo.s	$r4, 4($r1)
	sta.s	0x2006, $r0
	lda.s	$r5, 0x2006
	add	$r2, $r4
	add	$r2, $r5
	sta.l	0x0c0ffee0, $r2
	sta.b	0xdead, $r2
