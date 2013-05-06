	.section .text.boot
	.p2align	1
	.global	_start
	.global _exit
	.global msg
	.global bssvalue

	.equ BIG_ENDIAN,1

_start:	
	lda.s	$r0, .here
	sta.s	port_7seg_display, $r0

	# Set the top of stack and clear the frame pointer.
	ldi.l	$sp, _stack
	ldi.l	$r0, 0xEEEE
	lda.s	$r0, .here
.here:	xor	$fp, $fp

	ldi.l	$r0, 0xBBBB
	sta.s	port_7seg_display, $r0

.L1:
	# Clear out BSS
	ldi.l	$r0, __bss_start__
	ldi.l	$r1, __bss_end__
	xor	$r2, $r2

	ldi.l	$r3, 0xCCCC
	sta.s	port_7seg_display, $r3
	
.L3:
	cmp	$r0, $r1
	beq	.L4
	sto.l	0($r0), $r2

	ldi.l	$r3, 0xDDDD
	sta.s	port_7seg_display, $r0

	inc	$r0, 4
	jmpa	.L3

.L4:
	ldi.l	$r0, 0xFEED
	sta.s	port_7seg_display, $r0
	
	# Run main(int argc, char *argv[], char *env[])
	xor	$r0, $r0
	xor	$r1, $r1
	xor	$r2, $r2
	jsra	main

msg:   .string "moxie"	

	# This section defines the memory mapped I/O "ports"
	# starting at 0xF0000000.
	.section .ports

	.global port_7seg_display
	.global port_uart
	.global port_pic

port_7seg_display:
	.word	0 	# 0x00
	.word   0       # 0x02
	.word   0       # 0x04 ** UNUSED **
	.word   0       # 0x06 ** UNUSED **
port_uart:
	.word	0 	# 0x08 byte available
	.word	0 	# 0x0A ready to send
	.word	0 	# 0x0C rx byte (read only)
	.word	0 	# 0x0E tx byte (write only)
port_pic:
	.word   0	# 0x10 pending IRQs
	.word   0 	# 0x12 **UNUSED**
