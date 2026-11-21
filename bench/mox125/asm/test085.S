# B
	.include "asm/mmu.inc"
	.section .text
	.global	_start
_start:	
	# A store to a page that is not writable is a protection fault.
	ldi.l $r0, handler
	ssr $r0, 1
	mmu_setup
	tlb_write DATA, 0x50010000, 0x0c0fe000, TLB_V + TLB_G
	mmu_enter virt, ST_MMU
virt:
	ldi.l $r1, 0x12345678
	sta.l 0x50010000, $r1
	ldi.l $r1, 0xbad
	vfinish $r1

handler:
	gsr $r1, 2
	finish $r1
