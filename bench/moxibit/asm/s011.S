# ca1ecdb3
	/* Recursion through real stack frames: fact(10) with jsra, and
	   a call through a register, with push, pop and argument
	   registers saved on the stack.  */
	.include "mix.inc"
	.section .text
	.global	_start
_start:
	mixinit
	ldi.l	$sp, 0x8000
	ldi.l	$fp, 0
	ldi.l	$r0, 10
	jsra	fact
	mix	$r0
	mix	$sp
	mix	$fp
	ldi.l	$r4, twice
	ldi.l	$r0, 0x123
	jsr	$r4
	mix	$r0
	mix	$sp
	finish

	/* $r0 = $r0! */
fact:
	ldi.l	$r1, 1
	cmp	$r0, $r1
	bgt	1f
	ldi.l	$r0, 1
	ret
1:	push	$sp, $r0
	dec	$r0, 1
	jsra	fact
	pop	$sp, $r1
	mul	$r0, $r1
	ret

twice:
	push	$sp, $r5
	mov	$r5, $r0
	add	$r0, $r5
	pop	$sp, $r5
	ret
