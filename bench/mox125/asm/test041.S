# 1
	.section .text
	.global	_start
_start:
	/* taken branch immediately followed by an unconditional jump */
	xor     $r5, $r5
	xor     $r6, $r6
	cmp     $r5, $r6
	beq     _here
	jmpa    _bad
_here:
	inc     $r5, 1
	sta.l   0x0c0ffee0, $r5
	sta.b   0xdead, $r5
_bad:
	ldi.l   $r5, 0xbad
	sta.l   0x0c0ffee0, $r5
	sta.b   0xdead, $r5
