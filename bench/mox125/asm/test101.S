# 1
	.section .text
	.global	_start
_start:	
	# An opcode that means nothing is an exception, and the resume
	# address is the instruction itself.
	ldi.l	$r0, handler
	ssr	$r0, 1
	xor	$r1, $r1
bad:
	.word	0x3f00
	ldi.l	$r1, 0xbad
done:
	sta.l	0x0c0ffee0, $r1
	sta.b	0xdead, $r1
handler:
	gsr	$r2, 5
	ldi.l	$r3, bad
	cmp	$r2, $r3
	bne	done
	gsr	$r1, 2
	jmpa	done
