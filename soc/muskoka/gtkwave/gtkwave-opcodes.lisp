#!/usr/bin/sbcl --script

;;; --------------------------------------------------------------------
;;; GTKWave Process Filter to Disassemble Moxie Opcodes
;;;
;;; Copyright (c) 2011, 2012, 2016  Anthony Green.
;;; DO NOT ALTER OR REMOVE COPYRIGHT NOTICES.
;;; 
;;; The above named program is free software; you can redistribute it
;;; and/or modify it under the terms of the GNU General Public License
;;; version 2 as published by the Free Software Foundation.
;;; 
;;; The above named program is distributed in the hope that it will be
;;; useful, but WITHOUT ANY WARRANTY; without even the implied warranty
;;; of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;; 
;;; You should have received a copy of the GNU General Public License
;;; along with this work; if not, write to the Free Software
;;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA
;;; 02110-1301, USA.
;;; 
;;; --------------------------------------------------------------------

;; Return the name of the register identified by INDEX.
(defun regname (index)
  (nth index '("$fp" "$sp" "$r0" "$r1" 
	       "$r2" "$r3" "$r4" "$r5" 
	       "$r6" "$r7" "$r8" "$r9" 
	       "$r10" "$r11" "$r12" "$r13")))

;; ------------------------------------------------------
;; Disassemble the different instruction format types

(defun moxie-narg (name instr)
  name)

(defun moxie-f1-a (name instr)
  (format nil "~A ~A" name 
	  (regname (logand (ash instr -4) #xf))))

(defun moxie-f1-a4 (name instr)
  (format nil "~A ~A __" name 
	  (regname (logand (ash instr -4) #xf))))

(defun moxie-f1-4 (name instr)
  (format nil "~A __" name))

(defun moxie-f1-4a (name instr)
  (format nil "~A __ ~A" name
	  (regname (logand (ash instr -4) #xf))))

(defun moxie-f1-ab (name instr)
  (format nil "~A ~A ~A" name 
	  (regname (logand (ash instr -4) #xf))
	  (regname (logand instr #xf))))

(defun moxie-f1-m (name instr)
  name)

(defun moxie-f1-abi (name instr)
  (format nil "~A ~A (~A)" name 
	  (regname (logand (ash instr -4) #xf))
	  (regname (logand instr #xf))))

(defun moxie-f1-aib (name instr)
  (format nil "~A (~A) ~A" name 
	  (regname (logand (ash instr -4) #xf))
	  (regname (logand instr #xf))))

(defun moxie-f1-abi4 (name instr)
  (format nil "~A ~A __(~A)" name 
	  (regname (logand (ash instr -4) #xf))
	  (regname (logand instr #xf))))

(defun moxie-f1-aib4 (name instr)
  (format nil "~A __(~A) ~A" name 
	  (regname (logand (ash instr -4) #xf))
	  (regname (logand instr #xf))))

(defun moxie-f2-a8v (name instr)
  (format nil "~A ~A ~A" name 
	  (regname (logand (ash instr -8) #xf))
	  (logand instr #xf)))

(defun moxie-f3-pcrel (name instr)
  name)

;; ------------------------------------------------------
;; Populate the three opcode tables

(defmacro make-opcode-hash-table (spec-list)
  `(let ((table (make-hash-table)))
     (mapc #'(lambda (spec)
	       (destructuring-bind (code opcode fn) spec
		 (setf (gethash code table) (cons opcode fn))))
	   ,spec-list)
     table))

(defvar *form1-opcodes* 
  (make-opcode-hash-table
   '((#x00 "bad" moxie-narg)
     (#x01 "ldi.l" moxie-f1-a4)
     (#x02 "mov" moxie-f1-ab)
     (#x03 "jsra" moxie-f1-m )
     (#x04 "ret" moxie-narg)
     (#x05 "add.l" moxie-f1-ab)
     (#x06 "push" moxie-f1-ab)
     (#x07 "pop" moxie-f1-ab)
     (#x08 "lda.l" moxie-f1-a4)
     (#x09 "sta.l" moxie-f1-4a)
     (#x0a "ld.l" moxie-f1-abi)
     (#x0b "st.l" moxie-f1-aib)
     (#x0c "ldo.l" moxie-f1-abi4)
     (#x0d "sto.l" moxie-f1-aib4)
     (#x0e "cmp" moxie-f1-ab)
     (#x0f "nop" moxie-narg)
     (#x10 "sex.b" moxie-f1-ab)
     (#x11 "sex.s" moxie-f1-ab)
     (#x12 "zex.b" moxie-f1-ab)
     (#x13 "zex.s" moxie-f1-ab)
     (#x14 "bad" moxie-narg)
     (#x15 "bad" moxie-narg)
     (#x16 "bad" moxie-narg)
     (#x17 "bad" moxie-narg)
     (#x18 "bad" moxie-narg)
     (#x19 "jsr" moxie-f1-a)
     (#x1a "jmpa" moxie-f1-m )
     (#x1b "ldi.b" moxie-f1-a4)
     (#x1c "ld.b" moxie-f1-abi)
     (#x1d "lda.b" moxie-f1-a4)
     (#x1e "st.b" moxie-f1-aib)
     (#x1f "sta.b" moxie-f1-4a)
     (#x20 "ldi.s" moxie-f1-a4)
     (#x21 "ld.s" moxie-f1-abi)
     (#x22 "lda.s" moxie-f1-a4)
     (#x23 "st.s" moxie-f1-aib)
     (#x24 "sta.s" moxie-f1-4a)
     (#x25 "jmp" moxie-f1-a)
     (#x26 "and" moxie-f1-ab)
     (#x27 "lshr" moxie-f1-ab)
     (#x28 "ashl" moxie-f1-ab)
     (#x29 "sub.l" moxie-f1-ab)
     (#x2a "neg" moxie-f1-ab)
     (#x2b "or" moxie-f1-ab)
     (#x2c "not" moxie-f1-ab)
     (#x2d "ashr" moxie-f1-ab)
     (#x2e "xor" moxie-f1-ab)
     (#x2f "mul.l" moxie-f1-ab)
     (#x30 "swi" moxie-f1-4)
     (#x31 "div.l" moxie-f1-ab)
     (#x32 "udiv.l" moxie-f1-ab)
     (#x33 "mod.l" moxie-f1-ab)
     (#x34 "umod.l" moxie-f1-ab)
     (#x35 "brk" moxie-narg)
     (#x36 "ldo.b" moxie-f1-abi4)
     (#x37 "sto.b" moxie-f1-aib4)
     (#x38 "ldo.s" moxie-f1-abi4)
     (#x39 "sto.s" moxie-f1-aib4)
     (#x3a "bad" moxie-narg)
     (#x3b "bad" moxie-narg)
     (#x3c "bad" moxie-narg)
     (#x3d "bad" moxie-narg)
     (#x3e "bad" moxie-narg)
     (#x3f "bad" moxie-narg))))

(defvar *form2-opcodes*
  (make-opcode-hash-table
   '((#x00 "inc" moxie-f2-a8v)
     (#x01 "dec" moxie-f2-a8v)
     (#x02 "gsr" moxie-f2-a8v)
     (#x03 "ssr" moxie-f2-a8v))))

(defvar *form3-opcodes*
  (make-opcode-hash-table
   '((#x00 "beq" moxie-f3-pcrel)
     (#x01 "bne" moxie-f3-pcrel)
     (#x02 "blt" moxie-f3-pcrel)
     (#x03 "bgt" moxie-f3-pcrel)
     (#x04 "bltu" moxie-f3-pcrel)
     (#x05 "bgtu" moxie-f3-pcrel)
     (#x06 "bge" moxie-f3-pcrel)
     (#x07 "ble" moxie-f3-pcrel)
     (#x08 "bgeu" moxie-f3-pcrel)
     (#x09 "bleu" moxie-f3-pcrel)
     (#x0a "bad" moxie-narg)
     (#x0b "bad" moxie-narg)
     (#x0c "bad" moxie-narg)
     (#x0d "bad" moxie-narg)
     (#x0e "bad" moxie-narg)
     (#x0f "bad" moxie-narg))))

;; ------------------------------------------------------
;; Loop forever, reading opcodes from stdin and writing
;; disassembled instructions to stdout.

(defmacro moxie-disassemble (opcode insn table)
  `(format t "~A~%"
	   (let ((insn-spec (gethash ,insn ,table)))
	     (when insn-spec
	       "bad"
	       (funcall (cdr insn-spec)
			(car insn-spec)
			,opcode)))))

(loop
   (let* ((s (read-line))
	  (n (or (parse-integer s :radix 16 :junk-allowed t) s)))
    (with-open-file 
     (out "trace.txt" :direction :output
	  :if-exists :supersede)
      (format out s))
    (if (eql n s)
	(format t "~A~%" s) ; This is not a number.  Just spit it back.
	(progn 
	  (cond ( ;; Handle Form 1 opcodes
		 (eql (logand n #b1000000000000000) 0) 
		 (moxie-disassemble n (ash n -8) *form1-opcodes*))
		( ;; Handle Form 2 opcodes
		 (eql (logand n #b0100000000000000) 0)
		 (moxie-disassemble n (logand (ash n -12) 3) *form2-opcodes*))
		( ;; Handle Form 3 opcodes
		 t
		 (moxie-disassemble n (logand (ash n -10) 15) *form3-opcodes*)))))
    (finish-output)))

                                                                                
