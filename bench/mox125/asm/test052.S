# C0FFD00
	.section .text
	.global	_start
_start:	
	/* ret restores $sp */
	ldi.l $sp, 0x0c0ffd00
	ldi.l $r0, 1
	jsra fn
	inc $r0, 1
	sta.l 0x0c0ffee0, $sp
	sta.b 0xdead, $r0
fn:
	inc $r0, 4
	ret
