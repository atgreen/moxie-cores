# C0
	.include "asm/mmu.inc"
	.section .text
	.global	_start
_start:	
	# An exception from user mode lands in supervisor mode with translation
	# off, and the saved status remembers where it came from.
	ldi.l $r0, handler
	ssr $r0, 1
	mmu_setup TLB_U
	mmu_enter virt, ST_MMU + ST_UM
virt:
	swi 0
	ldi.l $r1, 0xbad
	vfinish $r1
handler:
	ldi.l $r2, ST_MMU + ST_UM
	gsr $r1, 4
	and $r1, $r2
	ldi.l $r3, 4
	ashl $r1, $r3
	gsr $r4, 0
	and $r4, $r2
	or $r1, $r4
	finish $r1
