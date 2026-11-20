# 1234
	.section .text
	.global	_start
_start:
	ldi.l   $r0, 0x12345678
	sta.l   0x0c0ffd00, $r0
	ldi.l   $r1, 0x0c0ffc00
	ldo.s   $r2, 0x100($r1)
	sta.l   0x0c0ffee0, $r2
	sta.b   0xdead, $r0
