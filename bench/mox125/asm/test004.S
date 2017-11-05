# 67890000
	.section .text
	.global	_start
_start:	
	ldi.s   $r0, 0x6789
	ldi.l   $r1, 0x0c0ffee0
	st.s    ($r1), $r0
	sta.b   0xdead, $r0
