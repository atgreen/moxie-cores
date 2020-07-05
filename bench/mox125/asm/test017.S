# 11112222
	.section .text
	.global	_start
_start:	
	ldi.l   $r0, 0x11112222
	jsra    fn
	ldi.l   $r0, 0xaaaaaaaa
fn:	sta.l   0x00c0ffee0, $r0
	sta.b   0xdead, $r0
