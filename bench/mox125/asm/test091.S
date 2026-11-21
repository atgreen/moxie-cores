# 2222
	.include "asm/mmu.inc"
	.section .text
	.global	_start
_start:	
	# A global entry is visible from every address space.
	mmu_setup
	ldi.l $r0, 9
	ssr $r0, 12
	mmu_enter virt, ST_MMU
virt:
	ldi.l $r1, 0x2222
	vfinish $r1
