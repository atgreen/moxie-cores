/* mdu-emulate.S - mul, div, udiv, mod and umod in software

   Copyright (c) 2026 Anthony Green.  All Rights Reserved.
   Distributed under the terms of the GNU General Public License
   version 2.

   moxibit built with MDU = 0 has no multiplier or divider: those five
   instructions are undefined, and raise the undefined instruction
   exception (cause 1) with sr5 pointing at them.  Point sr1 at
   mdu_emulate and it does what the instruction would have done and
   resumes after it, as if nothing had happened; the condition codes
   come back with the rest of the status word at eret.

   Anything else it is given - another cause, another undefined
   instruction, or a division by zero, for which it sets the cause to
   0 as the hardware would - goes to mdu_unhandled, which whoever
   includes this has to define, with every register as it was.

   It uses no stack, only the 64 bytes at mdu_regs: every register,
   $fp and $sp too, is saved there so that it can be an operand or
   the result.  */

	.section .text
	.global	mdu_emulate
mdu_emulate:
	sta.l	mdu_regs+0, $fp
	sta.l	mdu_regs+4, $sp
	sta.l	mdu_regs+8, $r0
	sta.l	mdu_regs+12, $r1
	sta.l	mdu_regs+16, $r2
	sta.l	mdu_regs+20, $r3
	sta.l	mdu_regs+24, $r4
	sta.l	mdu_regs+28, $r5
	sta.l	mdu_regs+32, $r6
	sta.l	mdu_regs+36, $r7
	sta.l	mdu_regs+40, $r8
	sta.l	mdu_regs+44, $r9
	sta.l	mdu_regs+48, $r10
	sta.l	mdu_regs+52, $r11
	sta.l	mdu_regs+56, $r12
	sta.l	mdu_regs+60, $r13

	gsr	$r0, 2
	ldi.l	$r1, 1
	cmp	$r0, $r1
	beq	1f
	jmpa	mdu_give_up

	/* $r1 = the opcode, $r2 and $r3 = where registers A and B are
	   saved, $r4 and $r5 = their values.  */
1:	gsr	$r0, 5
	ld.s	$r1, ($r0)
	ldi.l	$r5, 15
	mov	$r3, $r1
	and	$r3, $r5
	mov	$r2, $r1
	ldi.l	$r6, 4
	lshr	$r2, $r6
	and	$r2, $r5
	ldi.l	$r6, 8
	lshr	$r1, $r6
	ldi.l	$r6, 2
	ashl	$r2, $r6
	ashl	$r3, $r6
	ldi.l	$r6, mdu_regs
	add	$r2, $r6
	add	$r3, $r6
	ld.l	$r4, ($r2)
	ld.l	$r5, ($r3)

	/* $r12 = signed, $r13 = want the remainder.  */
	ldi.l	$r12, 1
	ldi.l	$r13, 0
	ldi.l	$r6, 0x2f
	cmp	$r1, $r6
	beq	mdu_mul
	ldi.l	$r6, 0x31
	cmp	$r1, $r6
	beq	mdu_div
	ldi.l	$r12, 0
	ldi.l	$r6, 0x32
	cmp	$r1, $r6
	beq	mdu_div
	ldi.l	$r12, 1
	ldi.l	$r13, 1
	ldi.l	$r6, 0x33
	cmp	$r1, $r6
	beq	mdu_div
	ldi.l	$r12, 0
	ldi.l	$r6, 0x34
	cmp	$r1, $r6
	beq	mdu_div
	jmpa	mdu_give_up

	/* $r4 * $r5, by shifting and adding.  */
mdu_mul:
	ldi.l	$r6, 0
	ldi.l	$r7, 0
	ldi.l	$r9, 1
1:	cmp	$r5, $r7
	beq	2f
	mov	$r8, $r5
	and	$r8, $r9
	cmp	$r8, $r7
	beq	3f
	add	$r6, $r4
3:	add	$r4, $r4
	lshr	$r5, $r9
	jmpa	1b
2:	mov	$r4, $r6
	jmpa	mdu_done

	/* $r4 / $r5 or $r4 % $r5: a restoring divider on the magnitudes,
	   negated afterwards as the hardware does.  $r11 = negate.  */
mdu_div:
	ldi.l	$r7, 0
	cmp	$r5, $r7
	bne	1f
	ssr	$r7, 2
	jmpa	mdu_give_up
1:	ldi.l	$r11, 0
	cmp	$r12, $r7
	beq	3f
	cmp	$r4, $r7
	bge	2f
	neg	$r4, $r4
	ldi.l	$r11, 1
2:	cmp	$r5, $r7
	bge	3f
	neg	$r5, $r5
	cmp	$r13, $r7
	bne	3f
	ldi.l	$r8, 1
	xor	$r11, $r8

	/* $r6 = the dividend, becoming the quotient; $r7 = the
	   remainder; $r10 counts the steps.  */
3:	mov	$r6, $r4
	ldi.l	$r7, 0
	ldi.l	$r10, 32
	ldi.l	$r12, 31
4:	mov	$r8, $r7
	lshr	$r8, $r12
	add	$r7, $r7
	mov	$r9, $r6
	lshr	$r9, $r12
	add	$r7, $r9
	add	$r6, $r6
	ldi.l	$r9, 0
	cmp	$r8, $r9
	bne	5f
	cmp	$r7, $r5
	bltu	6f
5:	sub	$r7, $r5
	inc	$r6, 1
6:	dec	$r10, 1
	cmp	$r10, $r9
	bne	4b

	mov	$r4, $r6
	cmp	$r13, $r9
	beq	7f
	mov	$r4, $r7
7:	cmp	$r11, $r9
	beq	mdu_done
	neg	$r4, $r4

	/* Store the result in register A, and resume after the
	   instruction.  The condition codes say which way to leave
	   once the registers are back: loads do not change them.  */
mdu_done:
	st.l	($r2), $r4
	gsr	$r0, 5
	inc	$r0, 2
	ssr	$r0, 5
	cmp	$r0, $r0
	jmpa	mdu_restore

mdu_give_up:
	ldi.l	$r0, 0
	ldi.l	$r1, 1
	cmp	$r0, $r1

mdu_restore:
	lda.l	$fp, mdu_regs+0
	lda.l	$sp, mdu_regs+4
	lda.l	$r0, mdu_regs+8
	lda.l	$r1, mdu_regs+12
	lda.l	$r2, mdu_regs+16
	lda.l	$r3, mdu_regs+20
	lda.l	$r4, mdu_regs+24
	lda.l	$r5, mdu_regs+28
	lda.l	$r6, mdu_regs+32
	lda.l	$r7, mdu_regs+36
	lda.l	$r8, mdu_regs+40
	lda.l	$r9, mdu_regs+44
	lda.l	$r10, mdu_regs+48
	lda.l	$r11, mdu_regs+52
	lda.l	$r12, mdu_regs+56
	lda.l	$r13, mdu_regs+60
	beq	1f
	jmpa	mdu_unhandled
1:	eret

	.section .data
	.p2align 2
mdu_regs:
	.space	64
