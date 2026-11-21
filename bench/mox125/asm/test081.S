# 1007
	.section .text
	.global	_start
_start:	
	/* brk enters the handler and resumes after itself */
	ldi.l $r0, handler
	ssr $r0, 1
	ldi.l $r5, 0x1000
	brk
	inc $r5, 2
	sta.l 0x0c0ffee0, $r5
	sta.b 0xdead, $r0
handler:
	gsr $r2, 2
	add $r5, $r2
	gsr $r3, 5
	jmp $r3
