# 22222222
	.section .text
	.global	_start
_start:
	/* back-to-back 32-bit stores from different registers */
	ldi.l   $r0, 0x11111111
	ldi.l   $r1, 0x22222222
	ldi.l   $r2, 0x33333333
	sta.l   0x0c0ffd00, $r0
	sta.l   0x0c0ffee0, $r1
	sta.l   0x0c0ffd04, $r2
	sta.b   0xdead, $r0
