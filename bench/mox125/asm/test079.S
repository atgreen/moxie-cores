# 11
	.section .text
	.global	_start
_start:	
	/* condition codes survive an interrupt taken between cmp and branch */
	ldi.l $r0, handler
	ssr $r0, 1
	xor $r5, $r5
	ldi.l $r1, 1
	ssr $r1, 0
	cmp $r5, $r5
	sta.s 0xf0000010, $r1
	beq good
	inc $r5, 0x40
good:
	inc $r5, 1
	sta.l 0x0c0ffee0, $r5
	sta.b 0xdead, $r0
handler:
	xor $r2, $r2
	sta.s 0xf0000010, $r2
	inc $r5, 0x10
	cmp $r1, $r2
	eret
