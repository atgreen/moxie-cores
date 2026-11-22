# cfb02678
	/* Shifts by 0, 1, 15, 31, 32, 33 and amounts with high bits
	   set, of a negative and a positive number.  */
	.include "mix.inc"

	.macro	shifts value, amount
	ldi.l	$r0, \value
	ldi.l	$r1, \amount
	mov	$r2, $r0
	lshr	$r2, $r1
	mix	$r2
	mov	$r2, $r0
	ashl	$r2, $r1
	mix	$r2
	mov	$r2, $r0
	ashr	$r2, $r1
	mix	$r2
	.endm

	.macro	amounts value
	shifts	\value, 0
	shifts	\value, 1
	shifts	\value, 15
	shifts	\value, 31
	shifts	\value, 32
	shifts	\value, 33
	shifts	\value, 0x80000001
	shifts	\value, 0xffffffff
	.endm

	.section .text
	.global	_start
_start:
	mixinit
	amounts	0x80000001
	amounts	0x7ffffffe
	/* A register shifted by itself.  */
	ldi.l	$r3, 3
	ashl	$r3, $r3
	mix	$r3
	finish
