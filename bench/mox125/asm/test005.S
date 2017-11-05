# 67000000
	.section .text
	.global	_start
_start:	
	ldi.b   $r0, 0x67
	ldi.l   $r1, 0x0c0ffee0
	st.b    ($r1), $r0
	sta.b   0xdead, $r0
