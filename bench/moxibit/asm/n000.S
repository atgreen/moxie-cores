# b
	/* On a core built with COMPACT = 0, the compact-code extension's
	   instructions are undefined: each resumes at itself with cause
	   1, and the handler counts it and skips it.  Only tb-nocompact
	   runs this.  */
	.section .text
	.global	_start
_start:
	xor	$r12, $r12
	ldi.l	$r0, handler
	ssr	$r0, 1
	.word	0x4000		/* ashli */
	.word	0x4600		/* cmpi */
	.word	0x4800		/* ldo.l, short */
	.word	0x5000		/* ldo.l, $fp */
	.word	0x5700		/* pushm */
	.word	0x5800		/* popm */
	.word	0x5a00		/* add3 */
	.word	0xe800		/* bra */
	.word	0xec00		/* beqz */
	.word	0xf000		/* ldi.l, short */
	.word	0xf800		/* bnez */
	sta.l	0x0c0ffee0, $r12
	sta.b	0xdead, $r12
handler:
	gsr	$r1, 2
	ldi.l	$r2, 1
	cmp	$r1, $r2
	bne	1f
	inc	$r12, 1
1:	gsr	$r1, 5
	inc	$r1, 2
	ssr	$r1, 5
	eret
