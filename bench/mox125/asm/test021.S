# 11112222
	.section .text
	.global	_start
_start:	
	ldi.l   $r0, 0x11112222
	nop
	nop
	nop
	nop
	jsra    fn
	nop
	nop
	nop
	nop
	sta.b   0xdead, $r0
	nop
	nop
	nop
	nop
	ldi.l   $r0, 0xaaaaaaaa
	nop
	nop
	nop
	nop
fn:	nop
	nop
	nop
	nop
	sta.l   0x00c0ffee0, $r0
	nop
	nop
	nop
	nop
	ret
