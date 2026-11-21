# C
	.include "asm/mmu.inc"
	.section .text
	.global	_start
_start:	
	# Special registers are off limits in user mode.
	ldi.l $r0, handler
	ssr $r0, 1
	mmu_setup TLB_U
	mmu_enter virt, ST_MMU + ST_UM
virt:
	gsr $r1, 0
	ldi.l $r1, 0xbad
	vfinish $r1

handler:
	gsr $r1, 2
	finish $r1
