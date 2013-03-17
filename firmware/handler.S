# Copyright (c) 2013, Anthony Green
# All rights reserved.

# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:

#  	Redistributions of source code must retain the above copyright
# 	notice, this list of conditions and the following disclaimer.

#  	Redistributions in binary form must reproduce the above
# 	copyright notice, this list of conditions and the following
# 	disclaimer in the documentation and/or other materials
# 	provided with the distribution.

# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
# CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
# INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
# MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
# DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR
# CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
# SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
# NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES ;
# LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
# HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
# CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
# OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE,
# EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

	.text
	.p2align 1
	.global __moxie_exception_handler
__moxie_exception_handler:
	
	/* Create return frame on current stack.  We'll fill the
	return address in later. */
	dec   $sp, 12
	st.l  ($sp), $fp

	/* Save all general purpose registers.  */
	mov   $fp, $sp
	push  $sp, $r0
	push  $sp, $r1
	push  $sp, $r2
	push  $sp, $r3
	push  $sp, $r4
	push  $sp, $r5
	push  $sp, $r6
	push  $sp, $r7
	push  $sp, $r8
	push  $sp, $r9
	push  $sp, $r10
	push  $sp, $r11
	push  $sp, $r12
	push  $sp, $r13

	/* Get our handler parameters ...
	   (*handler)(fault_address, fault_type, fault_code) */
	gsr   $r0, 5
	gsr   $r1, 2
	gsr   $r2, 3
	/* ENABLE INTERRUPTS HERE.  */
	
	jsra __handle_exception

	/* Insert the return address in our call frame.  */
	sto.l 4($fp), $r0
	
	mov   $r0, $fp
	dec   $r0, 14*4
	mov   $sp, $r0

	/* Restore all general purpose registers.  */

	pop   $sp, $r13
	pop   $sp, $r12
	pop   $sp, $r11
	pop   $sp, $r10
	pop   $sp, $r9
	pop   $sp, $r8
	pop   $sp, $r7
	pop   $sp, $r6
	pop   $sp, $r5
	pop   $sp, $r4
	pop   $sp, $r3
	pop   $sp, $r2
	pop   $sp, $r1
	pop   $sp, $r0
	
	ret

