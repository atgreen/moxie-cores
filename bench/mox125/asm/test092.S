# 10021000
	.include "asm/mmu.inc"
	.section .text
	.global	_start
_start:	
	# A call whose stack page is not mapped faults on its first write and is
	# restarted with the stack pointer untouched.
	ldi.l $r0, handler
	ssr $r0, 1
	mmu_setup
	ldi.l $sp, 0x10021000
	mmu_enter virt, ST_MMU
virt:
	jsra fn
	mov $r1, $sp
	vfinish $r1
fn:
	ret

handler:
	gsr $r13, 2
	ldi.l $r12, 8
	cmp $r13, $r12
	bne bad
	ldi.l $r12, 0x0c0fd000 + TLB_V + TLB_W + TLB_G
	ssr $r12, 10
	xor $r12, $r12
	ssr $r12, 11
	eret
bad:
	ldi.l $r1, 0xbad
	finish $r1
