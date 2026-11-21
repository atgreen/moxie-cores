# 1751
	.include "asm/mmu.inc"
	.section .text
	.global	_start
_start:	
	# A cas.l whose write faults has changed nothing and is restarted.  The
	# page starts out read-only; the handler makes it writable and counts.
	ldi.l $r0, handler
	ssr $r0, 1
	ldi.l $r0, 5
	sta.l 0x0c0fed00, $r0
	xor $r6, $r6
	mmu_setup
	tlb_write DATA, 0x50010000, 0x0c0fe000, TLB_V + TLB_G
	mmu_enter virt, ST_MMU
virt:
	ldi.l $r1, 0x50010d00
	ldi.l $r0, 5
	ldi.l $r2, 7
	cas.l ($r1), $r2
	
	beq swapped
	xor $r5, $r5
	jmpa report
swapped:
	ldi.l $r5, 1
report:
	ldi.l $r3, 4
	ashl $r0, $r3
	or $r5, $r0
	lda.l $r4, 0x50010d00
	ldi.l $r3, 8
	ashl $r4, $r3
	or $r5, $r4
	
	ldi.l $r3, 12
	ashl $r6, $r3
	or $r5, $r6
	vfinish $r5
handler:
	inc $r6, 1
	gsr $r13, 2
	ldi.l $r12, 11
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
