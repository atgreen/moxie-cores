# 11
	.include "asm/mmu.inc"
	.section .text
	.global	_start
_start:	
	# wfi wakes for a request that is already pending even though
	# interrupts are disabled, and the interrupt is taken once they
	# are enabled afterwards.
	ldi.l	$r0, handler
	ssr	$r0, 1
	xor	$r5, $r5
	ldi.l	$r1, 1
	sta.s	0xf0000010, $r1
	wfi
	ssr	$r1, 0
	inc	$r5, 1
	finish	$r5
handler:
	xor	$r2, $r2
	sta.s	0xf0000010, $r2
	inc	$r5, 0x10
	eret
