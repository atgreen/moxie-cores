# 12345678
	.include "asm/mmu.inc"
	.section .text
	.global	_start
_start:	
	# A store that misses the data TLB is restarted once the handler has
	# filled in the entry.
	ldi.l $r0, handler
	ssr $r0, 1
	tlb_clear
	tlb_write INSN, 0x00001000, 0x00001000, TLB_V + TLB_G
	tlb_write DATA, 0x0000d000, 0x0000d000, TLB_V + TLB_W + TLB_G
	mmu_enter virt, ST_MMU
virt:
	ldi.l $r1, 0x12345678
	vfinish $r1

handler:
	gsr $r13, 2
	ldi.l $r12, 8
	cmp $r13, $r12
	bne bad
	gsr $r13, 8
	ldi.l $r12, 0x40000ee0
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
