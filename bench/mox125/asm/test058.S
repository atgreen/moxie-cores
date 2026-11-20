# 3
	.section .text
	.global	_start
_start:	
	/* nested calls */
	ldi.l $sp, 0x0c0ffd00
	xor $r0, $r0
	jsra f1
	inc $r0, 1
	sta.l 0x0c0ffee0, $r0
	sta.b 0xdead, $r0
f1:
	jsra f2
	inc $r0, 1
	ret
f2:
	inc $r0, 1
	ret
