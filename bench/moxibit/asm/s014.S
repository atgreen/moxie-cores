# d8298e8f
	/* Undefined instructions, including the branch with condition
	   15, resume at themselves with cause 1; the handler skips
	   them.  brk resumes after itself with cause 5.  */
	.include "mix.inc"
	.section .text
	.global	_start
_start:
	mixinit
	ldi.l	$r0, handler
	ssr	$r0, 1
	.word	0x0000
	.word	0x1400
	.word	0x5900
	.word	0xfc00
	.word	0x3d00
	brk
	finish
handler:
	gsr	$r1, 2
	mix	$r1
	gsr	$r1, 5
	mix	$r1
	ldi.l	$r2, 5
	cmp	$r1, $r2
	gsr	$r1, 2
	cmp	$r1, $r2
	beq	1f
	gsr	$r1, 5
	inc	$r1, 2
	ssr	$r1, 5
1:	eret
