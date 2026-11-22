# 6de9a89a
	/* wfi carries on once the interrupt line is up, whether or not
	   interrupts are enabled; enabling them then takes the
	   interrupt straight away, and eret resumes after the ssr.  */
	.include "mix.inc"
	.section .text
	.global	_start
_start:
	mixinit
	ldi.l	$r0, handler
	ssr	$r0, 1
	ldi.l	$r1, 1
	sta.s	0xf0000010, $r1
	wfi
	inc	$r13, 0x40
	ldi.l	$r2, 0x101
	ssr	$r2, 0
	inc	$r13, 0x10
	gsr	$r3, 0
	mix	$r3
	gsr	$r3, 2
	mix	$r3
	gsr	$r3, 4
	mix	$r3
	finish
handler:
	xor	$r4, $r4
	sta.s	0xf0000010, $r4
	mix	$r13
	gsr	$r5, 5
	mix	$r5
	gsr	$r5, 0
	mix	$r5
	eret
