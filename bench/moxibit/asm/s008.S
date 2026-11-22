# 1b4e025e
	/* The ALU operations, sign and zero extension, inc and dec.  */
	.include "mix.inc"
	.section .text
	.global	_start
_start:
	mixinit
	ldi.l	$r0, 0x80f07f81
	ldi.l	$r1, 0x0ff0a5c3
	mov	$r2, $r0
	sub	$r2, $r1
	mix	$r2
	mov	$r2, $r0
	and	$r2, $r1
	mix	$r2
	mov	$r2, $r0
	or	$r2, $r1
	mix	$r2
	mov	$r2, $r0
	xor	$r2, $r1
	mix	$r2
	not	$r2, $r0
	mix	$r2
	neg	$r2, $r0
	mix	$r2
	neg	$r2, $r2
	mix	$r2
	sex.b	$r2, $r0
	mix	$r2
	sex.b	$r2, $r1
	mix	$r2
	sex.s	$r2, $r0
	mix	$r2
	sex.s	$r2, $r1
	mix	$r2
	zex.b	$r2, $r0
	mix	$r2
	zex.s	$r2, $r1
	mix	$r2
	ldi.l	$r2, 0xffffff80
	inc	$r2, 0xff
	mix	$r2
	dec	$r2, 0x81
	mix	$r2
	dec	$r2, 0
	mix	$r2
	ldi.b	$r3, 0x12345678
	mix	$r3
	ldi.s	$r3, 0x9abcdef0
	mix	$r3
	finish
