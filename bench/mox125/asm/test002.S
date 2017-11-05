# ab6789ab
	.section .text
	.global	_start
_start:	
	ldi.l   $r0, 0xab6789ab
	sta.l   0x0c0ffee0, $r0
	sta.b   0xdead, $r0
