# 751
	.section .text
	.global	_start
_start:	
	# cas.l writes the new value when memory holds what $r0 expects, and
	# leaves the old value in $r0 with the equal condition set.
	ldi.l $r1, 0x0c0ffd00
	ldi.l $r0, 5
	sta.l 0x0c0ffd00, $r0
	ldi.l $r2, 7
	cas.l ($r1), $r2
	
	beq swapped
	xor $r5, $r5
	jmpa report
swapped:
	ldi.l $r5, 1
report:
	ldi.l $r3, 4
	ashl $r0, $r3
	or $r5, $r0
	lda.l $r4, 0x0c0ffd00
	ldi.l $r3, 8
	ashl $r4, $r3
	or $r5, $r4
	
	sta.l 0x0c0ffee0, $r5
	sta.b 0xdead, $r5
