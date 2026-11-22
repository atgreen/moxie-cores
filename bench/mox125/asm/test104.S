# 16
	# The short loads, stores and loads of constants, lea and
	# %gprel.  $r12 counts the checks that pass, and the first that
	# fails ends the test.  Each check's expected value is loaded
	# with the long ldi.l.
	.macro	expect r, value
	.option	nocompact
	ldi.l	$r11, \value
	.option	compact
	cmp	\r, $r11
	beq	1f
	jmpa	done
1:	inc	$r12, 1
	.endm

	.data
	# _gp is here, at the start of .data.
d0:	.long	0x11111111
d1:	.long	0x22222222
	# buf[i] is 0xb0000000 + i.
buf:	.set	i, 0
	.rept	32
	.long	0xb0000000 + i
	.set	i, i + 1
	.endr

	.text
	.global	_start
_start:
	xor	$r12, $r12
	.option	compact

	# ldi.l: 2 bytes for -64..63, 4 for -32768..32767.
	ldi.l	$r1, 0
	expect	$r1, 0
	ldi.l	$r1, 63
	expect	$r1, 63
	ldi.l	$r1, -64
	expect	$r1, -64
	ldi.l	$r1, -1
	expect	$r1, -1
	ldi.l	$r1, 64
	expect	$r1, 64
	ldi.l	$r1, -32768
	expect	$r1, -32768
	ldi.l	$r1, 32767
	expect	$r1, 32767

	# Loads and stores with 3-bit registers and offsets 0-60.
	.option	nocompact
	ldi.l	$r2, buf
	.option	compact
	ldo.l	$r0, 0($r2)
	expect	$r0, 0xb0000000
	ldo.l	$r7, 60($r2)
	expect	$r7, 0xb000000f
	ldo.l	$r3, 36($r2)
	expect	$r3, 0xb0000009
	ldi.l	$r4, 0x12345678
	sto.l	20($r2), $r4
	.option	nocompact
	ldo.l	$r5, 20($r2)
	.option	compact
	expect	$r5, 0x12345678
	sto.l	44($r2), $r7
	.option	nocompact
	ldo.l	$r5, 44($r2)
	.option	compact
	expect	$r5, 0xb000000f

	# Loads and stores at $fp - 4 to $fp - 128, any register.
	.option	nocompact
	ldi.l	$fp, buf + 128
	.option	compact
	ldo.l	$r9, -4($fp)
	expect	$r9, 0xb000001f
	ldo.l	$r10, -128($fp)
	expect	$r10, 0xb0000000
	ldo.l	$r10, -68($fp)
	expect	$r10, 0xb000000f
	sto.l	-8($fp), $r9
	.option	nocompact
	ldo.l	$r5, -8($fp)
	.option	compact
	expect	$r5, 0xb000001f
	sto.l	-100($fp), $r4
	.option	nocompact
	ldo.l	$r5, -100($fp)
	.option	compact
	expect	$r5, 0x12345678

	# lea.
	lea	$r1, -12($fp)
	expect	$r1, buf + 116
	lea	$r1, 1000($r2)
	expect	$r1, buf + 1000

	# Offsets from _gp in $r13.
	.option	nocompact
	ldi.l	$r13, _gp
	.option	compact
	ldo.l	$r0, %gprel(d1)($r13)
	expect	$r0, 0x22222222
	sto.l	%gprel(d0)($r13), $r4
	lda.l	$r5, d0
	expect	$r5, 0x12345678
	lea	$r6, %gprel(buf+8)($r13)
	expect	$r6, buf + 8
done:
	sta.l	0x0c0ffee0, $r12
	sta.b	0xdead, $r12
