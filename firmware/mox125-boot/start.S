/* start.S - where the mox125 boot loader begins, and how it leaves.

   Copyright (c) 2026  Anthony Green

   The above named program is free software; you can redistribute it
   and/or modify it under the terms of the GNU General Public License
   version 2 as published by the Free Software Foundation.

   The above named program is distributed in the hope that it will be
   useful, but WITHOUT ANY WARRANTY; without even the implied warranty
   of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this work; if not, write to the Free Software
   Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA
   02110-1301, USA.  */

	.section .text.boot
	.p2align	1
	.global	_start
_start:
	/* The core starts here, in supervisor mode with interrupts and
	   translation off.  */
	ldi.l	$sp, _stack
	xor	$fp, $fp

	ldi.l	$r0, __bss_start__
	ldi.l	$r1, __bss_end__
	xor	$r2, $r2
1:	cmp	$r0, $r1
	beq	2f
	st.l	($r0), $r2
	inc	$r0, 4
	jmpa	1b
2:
	/* The ABI has every caller leave room for six argument
	   registers.  */
	dec	$sp, 24
	jsra	main
3:	jmpa	3b

	/* void start_program (unsigned long address)

	   What was just loaded may be where something else was a moment
	   ago, so forget every instruction fetched so far.  A write to
	   special register 14 does that.  */
	.global	start_program
start_program:
	ssr	$r0, 14
	jmp	$r0
