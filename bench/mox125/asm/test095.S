# 121
	.include "asm/mmu.inc"
	.section .text
	.global	_start
_start:	
	# The instruction cache keeps untranslated and translated code apart,
	# and forgets a translation when it is flushed.  The same address holds
	# a different routine once it is mapped elsewhere.
	ldi.l $r0, handler
	ssr $r0, 1
	mmu_setup
	tlb_write INSN, 0x00002000, 0x00003000, TLB_V + TLB_G
	xor $r5, $r5
	ldi.l $r3, 4
	ldi.l $r6, back1
	jmpa routine
back1:
	mmu_enter virt, ST_MMU
virt:
	ldi.l $r6, back2
	jmpa routine
back2:
	swi 0
handler_return:
	ldi.l $r6, back3
	jmpa routine
back3:
	vfinish $r5

handler:
	# Point the page back at itself; the cache must be told.
	tlb_write INSN, 0x00002000, 0x00002000, TLB_V + TLB_G
	ssr $r12, 14
	mmu_enter handler_return, ST_MMU

.org 0x1000
routine:
	ashl $r5, $r3
	inc $r5, 1
	jmp $r6

.org 0x2000
	ashl $r5, $r3
	inc $r5, 2
	jmp $r6
