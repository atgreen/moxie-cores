# 12345678
	.include "asm/mmu.inc"
	.section .text
	.global	_start
_start:	
	# A store through a translated address lands on the mapped page.
	mmu_setup
	mmu_enter virt, ST_MMU
virt:
	ldi.l $r1, 0x12345678
	vfinish $r1
