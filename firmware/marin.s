	.text
	.p2align	1
	.global	MarinDisplayTest
	.global	main
	.global _exit
	.global __data_start__
	.global __data_end__
	.global __data_load__

	.equ BIG_ENDIAN,1
	.equ DISPLAY_ADDR,0x00100000
	.equ UART_ADDR,   0x01000000


_start:	
main:	
MarinDisplayTest:
	ldi.l	$r1, 72
	sta.b	UART_ADDR, $r1
	ldi.l	$r1, 69
	sta.b	UART_ADDR, $r1
	ldi.l	$r1, 76
	sta.b	UART_ADDR, $r1
	ldi.l	$r1, 76
	sta.b	UART_ADDR, $r1
	ldi.l	$r1, 79
	sta.b	UART_ADDR, $r1
	ldi.l	$r1, 33
	sta.b	UART_ADDR, $r1
	ldi.l	$r1, 0x12345678
	ldi.l	$r3, 0x0
loop:	sta.s	DISPLAY_ADDR, $r1
	dec 	$r1, 1
	ldi.l	$r2, 50000
loop2:	dec	$r2, 1
	cmp	$r2, $r3
	bne	loop2
	jmpa	loop

_exit:	jmpa _exit

/* 	.global __copy_ram
;; __copy_ram:
;; 	ldi.l	$r0, 0x5555
;; 	sta.s	DISPLAY_ADDR, $r0
;; 	ldi.l	$r0, __data_start__
;; 	ldi.l	$r1, __data_load__
;; 	ldi.l	$r2, __data_end__ - __data_start__
;; 	jsra	memcpy
;; 	ret

;; 	.data
;; 	.word	0 */
