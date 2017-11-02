# 0
	.section .text
	.global	_start
_start:	
	/* End simulation by writing to 0x00c0ffee */
	ldi.b   $r0, 0xff
	sta.s   0x00c0ffee, $r0
