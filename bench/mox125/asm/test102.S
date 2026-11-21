# 10
	.section .text
	.global	_start
_start:	
	# Dividing by zero is an exception, cause 0, and leaves the
	# dividend alone.
	ldi.l	$r0, handler
	ssr	$r0, 1
	ldi.l	$r1, 0x10
	xor	$r2, $r2
	div	$r1, $r2
	ldi.l	$r1, 0xbad
done:
	sta.l	0x0c0ffee0, $r1
	sta.b	0xdead, $r1
handler:
	gsr	$r3, 2
	add	$r1, $r3
	jmpa	done
