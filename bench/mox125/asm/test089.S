# 12345678
	.include "asm/mmu.inc"
	.section .text
	.global	_start
_start:	
	# Jumping to an address with no instruction mapping misses the
	# instruction TLB; the handler maps it and the code runs there.
	ldi.l $r0, handler
	ssr $r0, 1
	mmu_setup
	mmu_enter virt, ST_MMU
virt:
	ldi.l $r0, high + 0x80000000
	jmp $r0
high:
	ldi.l $r1, 0x12345678
	vfinish $r1
handler:
	gsr $r13, 2
	ldi.l $r12, 6
	cmp $r13, $r12
	bne bad
	ldi.l $r12, 0x00001000 + TLB_V + TLB_G
	ssr $r12, 10
	ldi.l $r12, INSN
	ssr $r12, 11
	eret
bad:
	ldi.l $r1, 0xbad
	finish $r1
