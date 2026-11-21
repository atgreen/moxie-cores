# 12345678
	.include "asm/mmu.inc"
	.section .text
	.global	_start
_start:	
	# A load that misses the data TLB is restarted the same way.
	ldi.l $r0, handler
	ssr $r0, 1
	ldi.l $r1, 0x12345678
	sta.l 0x0c0ffd00, $r1
	xor $r1, $r1
	tlb_clear
	tlb_write INSN, 0x00001000, 0x00001000, TLB_V + TLB_G
	tlb_write DATA, 0x0000d000, 0x0000d000, TLB_V + TLB_W + TLB_G
	mmu_enter virt, ST_MMU
virt:
	lda.l $r1, 0x40000d00
	vfinish $r1

handler:
	gsr $r13, 2
	ldi.l $r12, 7
	cmp $r13, $r12
	bne bad
	gsr $r13, 8
	ldi.l $r12, 0x40000d00
	cmp $r13, $r12
	bne bad
	# tlbhi already names the page that missed
	ldi.l $r12, 0x0c0ff000 + TLB_V + TLB_W + TLB_G
	ssr $r12, 10
	xor $r12, $r12
	ssr $r12, 11
	eret
bad:
	ldi.l $r1, 0xbad
	finish $r1
