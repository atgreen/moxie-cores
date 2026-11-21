# 51
	.include "asm/mmu.inc"
	.section .text
	.global	_start
_start:	
	# An interrupt taken in user mode with translation on is handled with
	# translation off and returns to user mode.
	ldi.l $r0, handler
	ssr $r0, 1
	mmu_setup TLB_U
	tlb_write DATA, 0xf0000000, 0xf0000000, TLB_V + TLB_W + TLB_G + TLB_U
	xor $r5, $r5
	mmu_enter virt, ST_MMU + ST_UM + ST_IE
virt:
	ldi.l $r1, 1
	sta.s 0xf0000010, $r1
	inc $r5, 1
	swi 0
handler:
	gsr $r13, 2
	ldi.l $r12, 2
	cmp $r13, $r12
	bne done
	xor $r2, $r2
	sta.s 0xf0000010, $r2
	inc $r5, 0x50
	eret
done:
	finish $r5
