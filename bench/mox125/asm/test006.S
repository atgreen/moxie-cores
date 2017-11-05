# 6789abcd
	.section .text
	.global	_start
_start:	
	ldi.l   $r0, 0x6789abcd
	ldi.l   $r1, 0x0c0ffee0
	st.l    ($r1), $r0
	sta.b   0xdead, $r0
