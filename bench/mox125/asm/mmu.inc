# Shared helpers for the memory management tests.
#
# Each test runs from physical address 0x1000.  Once translation is on,
# the result page 0x0c0ff000 is reached through virtual 0x40000000 and
# the exit address 0xdead through an identity mapping.

	.equ	TLB_V, 1		# valid
	.equ	TLB_W, 2		# writable
	.equ	TLB_U, 8		# user may use it
	.equ	TLB_G, 16		# matches every address space id

	.equ	ST_IE, 1		# interrupts enabled
	.equ	ST_UM, 4		# user mode
	.equ	ST_MMU, 8		# translation on

	.equ	DATA, 0
	.equ	INSN, 1

	# Write one TLB entry.  Clobbers $r12.
	.macro	tlb_write which, vaddr, paddr, flags
	ldi.l	$r12, \vaddr
	ssr	$r12, 9
	ldi.l	$r12, \paddr + \flags
	ssr	$r12, 10
	ldi.l	$r12, \which
	ssr	$r12, 11
	.endm

	# Make every entry of both TLBs invalid.  Clobbers $r10-$r13.
	.macro	tlb_clear
	xor	$r12, $r12
	ssr	$r12, 10
	ldi.l	$r11, 64
	ldi.l	$r13, 0x1000
9:	ssr	$r12, 9
	xor	$r10, $r10
	ssr	$r10, 11
	inc	$r10, 1
	ssr	$r10, 11
	add	$r12, $r13
	dec	$r11, 1
	xor	$r10, $r10
	cmp	$r11, $r10
	bne	9b
	.endm

	# The mappings every translated test needs: the code page, the
	# result page and the exit page.
	.macro	mmu_setup flags=0
	tlb_clear
	tlb_write INSN, 0x00001000, 0x00001000, TLB_V + TLB_G + \flags
	tlb_write DATA, 0x40000000, 0x0c0ff000, TLB_V + TLB_W + TLB_G + \flags
	tlb_write DATA, 0x0000d000, 0x0000d000, TLB_V + TLB_W + TLB_G + \flags
	.endm

	# Continue at TARGET with STATUS as the status register.
	.macro	mmu_enter target, status
	ldi.l	$r12, \status
	ssr	$r12, 4
	ldi.l	$r12, \target
	ssr	$r12, 5
	eret
	.endm

	# Report REG as the result and stop, with translation off.
	.macro	finish reg
	sta.l	0x0c0ffee0, \reg
	sta.b	0xdead, \reg
	.endm

	# The same with translation on.
	.macro	vfinish reg
	sta.l	0x40000ee0, \reg
	sta.b	0xdead, \reg
	.endm
