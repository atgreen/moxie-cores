# e0ac3db2
	/* What the special registers keep, the status word after cmp,
	   and branches on condition codes that ssr set by hand.  */
	.include "mix.inc"

	.macro	keeps n
	ldi.l	$r0, 0xffffffff
	ssr	$r0, \n
	gsr	$r1, \n
	mix	$r1
	.endm

	.macro	try cond
	\cond	1f
	jmpa	2f
1:	add	$r13, $r6
2:	add	$r6, $r6
	.endm

	.section .text
	.global	_start
_start:
	mixinit
	keeps	1
	keeps	2
	keeps	3
	keeps	4
	keeps	5
	keeps	6
	keeps	7
	gsr	$r1, 8
	mix	$r1
	ldi.l	$r2, 3
	ldi.l	$r3, -5
	cmp	$r2, $r3
	gsr	$r1, 0
	mix	$r1
	/* LT and GT at once, and LTU.  */
	ldi.l	$r0, 0x0e0
	ssr	$r0, 0
	ldi.l	$r6, 1
	try	beq
	try	bne
	try	blt
	try	bgt
	try	bltu
	try	bgtu
	try	bge
	try	ble
	try	bgeu
	try	bleu
	finish
