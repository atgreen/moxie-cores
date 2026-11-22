# 98024993
	/* mul, div, udiv, mod and umod at their edges.  */
	.include "mix.inc"

	.macro	try op, x, y
	ldi.l	$r0, \x
	ldi.l	$r1, \y
	\op	$r0, $r1
	mix	$r0
	.endm

	.section .text
	.global	_start
_start:
	mixinit
	try	mul, 0xfffffff9, 0xfffffffd
	try	mul, 0x10000, 0x10000
	try	mul, 0xffffffff, 0xffffffff
	try	mul, 0x12345678, 0
	try	mul, 0, 0x12345678
	try	mul, 0x9e3779b9, 0x7f4a7c15
	try	div, 0x80000000, 0xffffffff
	try	div, -7, 2
	try	div, 7, -2
	try	div, -7, -2
	try	div, 5, 7
	try	mod, -7, 2
	try	mod, 7, -2
	try	mod, -7, -2
	try	mod, 5, 7
	try	mod, 0x80000000, 0xffffffff
	try	udiv, 0xffffffff, 3
	try	udiv, 0x80000000, 0xffffffff
	try	udiv, 0xfffffffe, 0xffffffff
	try	udiv, 0x12345678, 1
	try	umod, 0xffffffff, 10
	try	umod, 0x80000000, 0xffffffff
	try	umod, 0xffffffff, 0x80000000
	/* The same register as both operands.  */
	ldi.l	$r2, 0x1234
	mul	$r2, $r2
	mix	$r2
	udiv	$r2, $r2
	mix	$r2
	finish
