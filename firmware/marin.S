	.text
	.p2align	1
	.global	_start
	.global _exit

	.global bssvalue

	.equ BIG_ENDIAN,1

	.equ DISPLAY_ADDR,0x00100000
	.equ UART_ADDR,   0x01000000


_start:	
	# Set the top of stack and clear the frame pointer.
	ldi.l	$sp, _stack
	xor	$fp, $fp

	# Copy data section from ROM to RAM
	ldi.l	$r0, __data_start__
	ldi.l	$r1, __data_load__
	ldi.l	$r2, __data_end__
	sub.l	$r2, $r0
	xor	$r3, $r3
.L0:
	cmp	$r2, $r3
	beq	.L1
	ldo.l	$r4, 0($r1)
	sto.l	0($r0), $r4
	inc	$r0, 4
	inc	$r1, 4
	dec	$r2, 1
	jmpa	.L0

.L1:
	# Clear out BSS
	ldi.l	$r0, __bss_start__
	ldi.l	$r1, __bss_end__
	xor	$r2, $r2
.L3:
	cmp	$r0, $r1
	beq	.L4
	sto.l	0($r0), $r2
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

_exit:	jmpa _exit

	# This section defines the memory mapped I/O "ports"
	# starting at 0xF0000000.
	.section .ports

	.global port_7seg_display
	.global port_uart

	port_7seg_display:		.word	0 	# 0x00
	                                .word   0       # 0x02
	port_uart:			.word	0 	# 0x04
					.word	0 	# 0x06
	
