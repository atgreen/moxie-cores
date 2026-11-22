# 14
	.section .text
	.global	_start
_start:
	# The compact-code instructions: shifts by an immediate, cmpi
	# and bra.  $r12 counts the checks that pass, and the first
	# that fails ends the test.
	.macro	expect r, value
	ldi.l	$r13, \value
	cmp	\r, $r13
	beq	1f
	jmpa	done
1:	inc	$r12, 1
	.endm

	.macro	shift op, value, count, result
	ldi.l	$r1, \value
	\op	$r1, \count
	expect	$r1, \result
	.endm

	xor	$r12, $r12
	shift	ashli, 0x80000001, 0, 0x80000001
	shift	ashli, 0x80000001, 31, 0x80000000
	shift	ashli, 0x12345678, 4, 0x23456780
	shift	ashli, 0x12345678, 16, 0x56780000
	shift	lshri, 0x80000001, 31, 1
	shift	lshri, 0x87654321, 4, 0x08765432
	shift	lshri, 0x87654321, 17, 0x43b2
	shift	ashri, 0x87654321, 4, 0xf8765432
	shift	ashri, 0x87654321, 31, 0xffffffff
	shift	ashri, 0x47654321, 20, 0x476

	# cmpi sets the same conditions as cmp, against its immediate
	# sign-extended, and leaves the register alone.
	.macro	compare value, imm, branch
	ldi.l	$r2, \value
	cmpi	$r2, \imm
	\branch	1f
	jmpa	done
1:	expect	$r2, \value
	.endm

	compare	5, 5, beq
	compare	-1, -1, beq
	compare	-16, -16, beq
	compare	15, 15, beq
	compare	3, -1, bgt
	compare	3, -1, bltu
	compare	16, 15, bgtu
	compare	-17, -16, blt

	# bra forward, then back.
	bra	2f
	jmpa	done
3:	inc	$r12, 1
	bra	done
2:	inc	$r12, 1
	bra	3b
done:
	sta.l	0x0c0ffee0, $r12
	sta.b	0xdead, $r12
