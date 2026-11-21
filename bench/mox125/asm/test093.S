# 20002
	.include "asm/mmu.inc"
	.section .text
	.global	_start
_start:	
	# A call frame that straddles two pages faults part way through, after
	# the return address has been written, and still restarts cleanly.
	# The result counts the two faults and checks the stack pointer.
	ldi.l $r0, handler
	ssr $r0, 1
	mmu_setup
	xor $r5, $r5
	ldi.l $sp, 0x10021008
	mmu_enter virt, ST_MMU
virt:
	jsra fn
	ldi.l $r2, 0x10021008
	cmp $r2, $sp
	bne wrong
	ldi.l $r2, 16
	mov $r1, $r5
	ashl $r1, $r2
	or $r1, $r5
	vfinish $r1
wrong:
	ldi.l $r1, 0xbad
	vfinish $r1
fn:
	ret
handler:
	inc $r5, 1
	# Map whichever of the two stack pages missed onto its own frame.
	gsr $r13, 8
	ldi.l $r12, 0x10021000
	cmp $r13, $r12
	blt low
	ldi.l $r12, 0x0c0fd000 + TLB_V + TLB_W + TLB_G
	jmpa map
low:
	ldi.l $r12, 0x0c0fc000 + TLB_V + TLB_W + TLB_G
map:
	ssr $r12, 10
	xor $r12, $r12
	ssr $r12, 11
	eret
