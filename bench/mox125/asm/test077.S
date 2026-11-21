# 1238
	.section .text
	.global	_start
_start:	
	/* swi enters the handler with its number and cause, and resumes after itself */
	ldi.l $r0, handler
	ssr $r0, 1
	xor $r5, $r5
	swi 0x1234
	inc $r5, 1
	sta.l 0x0c0ffee0, $r5
	sta.b 0xdead, $r0
handler:
	gsr $r5, 3
	gsr $r2, 2
	add $r5, $r2
	gsr $r3, 5
	jmp $r3
