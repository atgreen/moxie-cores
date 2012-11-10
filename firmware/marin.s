	.text
	.p2align	1
	.global	MarinDisplayTest

	.equ BIG_ENDIAN,1
	.equ DISPLAY_ADDR,0x00100000

MarinDisplayTest:
	ldi.l	$r1, 0x12345678
	ldi.l	$r3, 0x0
loop:	sta.s	DISPLAY_ADDR, $r1
	dec 	$r1, 1
	ldi.l	$r2, 5000000
loop2:	dec	$r2, 1
	cmp	$r2, $r3
	bne	loop2
	jmpa	loop+0x1000
