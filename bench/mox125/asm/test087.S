# A
	.include "asm/mmu.inc"
	.section .text
	.global	_start
_start:	
	# User mode cannot read a page that is not marked for it.
	ldi.l $r0, handler
	ssr $r0, 1
	mmu_setup TLB_U
	tlb_write DATA, 0x50010000, 0x0c0fe000, TLB_V + TLB_W + TLB_G
	mmu_enter virt, ST_MMU + ST_UM
virt:
	lda.l $r1, 0x50010000
	ldi.l $r1, 0xbad
	vfinish $r1

handler:
	gsr $r1, 2
	finish $r1
