# 11112222
	.section .text
	.global	_start
_start:	
	ldi.l   $r0, 0x11112222
	jmpa    L1
L2:	sta.b   0xdead, $r0 # stop here
L1:	sta.l   0x00c0ffee0, $r0
	jmpa    L2
