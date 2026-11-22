# 25af05a6
	/* Every branch condition against three comparisons: -1 with 1,
	   5 with 5, and 1 with 0x80000000.  Each taken branch sets the
	   next bit of $r5.  */
	.macro	try cond
	\cond	1f
	jmpa	2f
1:	add	$r5, $r6
2:	add	$r6, $r6
	.endm

	.macro	all x, y
	ldi.l	$r0, \x
	ldi.l	$r1, \y
	cmp	$r0, $r1
	try	beq
	try	bne
	try	blt
	try	bgt
	try	bltu
	try	bgtu
	try	bge
	try	ble
	try	bgeu
	try	bleu
	.endm

	.section .text
	.global	_start
_start:
	ldi.l	$r5, 0
	ldi.l	$r6, 1
	all	0xffffffff, 1
	all	5, 5
	all	1, 0x80000000
	sta.l	0x0c0ffee0, $r5
	sta.l	0xdead, $r5
