# 14
	# beqz, bnez, add3, pushm, popm and bsr.  $r12 counts the checks
	# that pass, and the first that fails ends the test.
	.macro	expect r, value
	ldi.l	$r11, \value
	cmp	\r, $r11
	beq	1f
	jmpa	done
1:	inc	$r12, 1
	.endm

	.text
	.global	_start
_start:
	xor	$r12, $r12
	ldi.l	$sp, 0x8000
	ldi.l	$fp, 0x8000

	# beqz and bnez, taken and not, forward and back.
	xor	$r0, $r0
	ldi.l	$r1, 7
	bnez	$r0, 2f
	beqz	$r1, 2f
	beqz	$r0, 3f
2:	jmpa	done
4:	inc	$r12, 1
	bra	5f
3:	inc	$r12, 1
	bnez	$r1, 4b
	jmpa	done
5:
	# add3, including a source that is the destination.
	ldi.l	$r1, 0x11111111
	ldi.l	$r2, 0x01020304
	add3	$r0, $r1, $r2
	expect	$r0, 0x12131415
	expect	$r1, 0x11111111
	add3	$r7, $r7, $r7
	ldi.l	$r7, 5
	add3	$r7, $r7, $r1
	expect	$r7, 0x11111116
	add3	$r3, $r2, $r3
	add3	$r3, $r2, $r2
	expect	$r3, 0x02040608

	# pushm stores $r2 at $sp - 4, ..., $r5 at $sp - 16; popm puts
	# them back and $sp where it was.
	ldi.l	$r2, 0xa2
	ldi.l	$r3, 0xa3
	ldi.l	$r4, 0xa4
	ldi.l	$r5, 0xa5
	pushm	$r2, $r5
	expect	$sp, 0x8000 - 16
	ldo.l	$r6, 12($sp)
	expect	$r6, 0xa2
	ldo.l	$r6, 0($sp)
	expect	$r6, 0xa5
	xor	$r2, $r2
	xor	$r3, $r3
	xor	$r4, $r4
	xor	$r5, $r5
	popm	$r2, $r5
	expect	$sp, 0x8000
	expect	$r2, 0xa2
	expect	$r3, 0xa3
	expect	$r5, 0xa5
	pushm	$r9, $r9
	expect	$sp, 0x8000 - 4

	# bsr calls as jsra does: the callee returns past it.
	ldi.l	$sp, 0x8000
	bsr	f
	expect	$r10, 0xf1
	expect	$sp, 0x8000
	expect	$fp, 0x8000

	# A taken branch squashes the pushm behind it, and a call lands
	# on one.
	ldi.l	$sp, 0x8000
	xor	$r0, $r0
	beqz	$r0, 6f
	pushm	$r2, $r5
6:	expect	$sp, 0x8000
	ldi.l	$r8, 0x88
	bsr	g
	expect	$r8, 0x88
	expect	$sp, 0x8000
done:
	sta.l	0x0c0ffee0, $r12
	sta.b	0xdead, $r12

	.section .text.f, "ax"
f:	ldi.l	$r10, 0xf1
	ret
g:	pushm	$r6, $r8
	xor	$r8, $r8
	popm	$r6, $r8
	ret
