# 1234abcd
	/* jmpa, and a store through a register.  */
	.section .text
	.global	_start
_start:
	ldi.l	$r0, 0x1234abcd
	jmpa	1f
	ldi.l	$r0, 0xbad
1:	ldi.l	$r1, 0x0c0ffee0
	st.l	($r1), $r0
	sta.l	0xdead, $r0
