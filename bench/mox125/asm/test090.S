# 806
	.include "asm/mmu.inc"
	.section .text
	.global	_start
_start:	
	# An entry made for one address space is invisible from another.
	ldi.l $r0, handler
	ssr $r0, 1
	mmu_setup
	ldi.l $r0, 5
	ssr $r0, 12
	tlb_write DATA, 0x50010005, 0x0c0fe000, TLB_V + TLB_W
	mmu_enter virt, ST_MMU
virt:
	ldi.l $r1, 0x1111
	sta.l 0x50010000, $r1
	swi 0
	ldi.l $r1, 0xbad
	vfinish $r1
handler:
	gsr $r13, 2
	ldi.l $r12, 3
	cmp $r13, $r12
	bne report
	# First time here, from the swi: move to address space 6 and try again.
	ldi.l $r12, 6
	ssr $r12, 12
	mmu_enter again, ST_MMU
again:
	sta.l 0x50010000, $r1
	ldi.l $r1, 0xbad
	vfinish $r1
report:
	# cause in the second digit, address space id of tlbhi in the last
	ldi.l $r12, 8
	ashl $r13, $r12
	gsr $r1, 9
	ldi.l $r12, 0xff
	and $r1, $r12
	or $r1, $r13
	finish $r1
