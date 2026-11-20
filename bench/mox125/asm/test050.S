# 6
	.section .text
	.global	_start
_start:	
	/* execution resumes after the call */
	ldi.l $sp, 0x0c0ffd00
	ldi.l $r0, 1
	jsra fn
	inc $r0, 1
	sta.l 0x0c0ffee0, $r0
	sta.b 0xdead, $r0
fn:
	inc $r0, 4
	ret
