# 12345678
	.include "asm/mmu.inc"
	.section .text
	.global	_start
_start:	
	# A 32-bit load whose two halves are on different pages faults on the
	# second half and is restarted from the first.
	ldi.l $r0, handler
	ssr $r0, 1
	ldi.l $r1, 0x12345678
	sta.l 0x0c0fdffe, $r1
	xor $r1, $r1
	mmu_setup
	tlb_write DATA, 0x50010000, 0x0c0fd000, TLB_V + TLB_W + TLB_G
	mmu_enter virt, ST_MMU
virt:
	lda.l $r1, 0x50010ffe
	vfinish $r1
handler:
	gsr $r13, 8
	ldi.l $r12, 0x50011000
	cmp $r13, $r12
	bne bad
	ldi.l $r12, 0x0c0fe000 + TLB_V + TLB_W + TLB_G
	ssr $r12, 10
	xor $r12, $r12
	ssr $r12, 11
	eret
bad:
	ldi.l $r1, 0xbad
	finish $r1
