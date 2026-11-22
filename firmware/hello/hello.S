# hello.S - say hello on the console of the mox125_board peripherals
#
# Copyright (c) 2026 Anthony Green.  All Rights Reserved.
# Distributed under the terms of the GNU General Public License
# version 2.
#
# For any core on the board that bench/mox125-board, tools/mox125-iss
# and cores/mox125_board share: RAM from address zero, the program at
# 0x1000 where the core starts, and the console at 0xF0000000.  It
# prints its message, writes to 0xF00000F2, which ends a simulation,
# and then stops.

	.section .text
	.global	_start
_start:
	ldi.l	$r0, message
	ldi.l	$r2, 0
1:	ld.b	$r1, ($r0)
	cmp	$r1, $r2
	beq	2f
	sta.b	0xf0000000, $r1
	inc	$r0, 1
	jmpa	1b
2:	sta.s	0xf00000f2, $r2
3:	jmpa	3b

message:
	.string	"Hello from moxie!\n"
