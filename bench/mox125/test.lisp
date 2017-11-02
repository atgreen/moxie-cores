;;; 1010101010101010101010101010101010101010101010101010101010101010101010101010101
;;; 0101010101010101010101010101010101010101010101010101010101010101010101010101010
;;; 1   ___  ___          _     ______                                            1
;;; 0   |  \/  |         (_)    |  _  \              Open Source Tools,           0
;;; 1   | .  . | _____  ___  ___| | | |_____   __    Firmware, and HDL code for   1
;;; 0   | |\/| |/ _ \ \/ / |/ _ \ | | / _ \ \ / /    the Moxie processor core.    0
;;; 1   | |  | | (_) >  <| |  __/ |/ /  __/\ V /                                  1
;;; 0   \_|  |_/\___/_/\_\_|\___|___/ \___| \_/      http://moxielogic.org/blog   0
;;; 1                                                                             1
;;; 0101010101010101010101010101010101010101010101010101010101010101010101010101010
;;; 1010101010101010101010101010101010101010101010101010101010101010101010101010101

;;; Copyright (C) 2017  Anthony Green <green@moxielogic.com>
;;; Distributed under the terms of the GPL v3 or later.

;;; This test program uses a verilator simulator of the mox125 moxie
;;; core, wrapped in a thin lisp veneer by wrapilator.

(ql:quickload :FiveAM)
(ql:quickload :elf)

;; this should really be inferred by :elf when it reads the elf binary
(setq elf:*endian* :big)

(load "obj_dir/verilated-moxie.lisp")
(use-package :verilated-moxie)
(use-package :it.bese.FiveAM)

(setq *test-dribble* t)
(setq *print-names* t)

;;; Create a new moxie
(defvar *cpu* (moxie-new))

;;; Clock up and eval
(defun tick-up ()
  (moxie-set-clk-i *cpu* 1)
  (moxie-eval *cpu*))

;;; Clock down and eval
(defun tick-down ()
  (moxie-set-clk-i *cpu* 0)
  (moxie-eval *cpu*))

;;; Hold reset high for CYCLES
(defun reset-cycles (cycles)
  (moxie-set-rst-i *cpu* 1)
  (loop for i from 1 to cycles do
       (tick-up)
       (tick-down))
  (moxie-set-rst-i *cpu* 0))

;;; ---------------------------------------------------------------------------
;;; After reset, the core should start reading from 0x1000
;;; ---------------------------------------------------------------------------
(test check-boot-address
      (moxie-set-wb-ack-i *cpu* 0)
      (reset-cycles 10)
      (loop until (and (eq (moxie-get-wb-stb-o *cpu*) 1)
		       (eq (moxie-get-wb-cyc-o *cpu*) 1))
	 do (progn
	      (tick-up)
	      (tick-down)))
      (is (= (moxie-get-wb-adr-o *cpu*) 4096)))

;;; ---------------------------------------------------------------------------
;;; Execute 4k of NOP instructions, making sure we end up at the expected $PC.
;;; Don't execute more than a fixed large number of instructions.
;;; Run test multiple times, with increasing numbers of memory wait states.
;;; ---------------------------------------------------------------------------
(test run-nop-sequence
      (loop for memory-wait-cycles from 0 to 120
	 do
	   (progn
	     (moxie-set-wb-ack-i *cpu* 0)
	     (reset-cycles 10)
	     (loop for count from 1 to 99999
		until (>= (moxie-get-wb-adr-o *cpu*) 8192)
		do
		  (progn
		    ;; Wait until there is a bus request...
		    (tick-up)
		    (loop until (and (eq (moxie-get-wb-stb-o *cpu*) 1)
				     (eq (moxie-get-wb-cyc-o *cpu*) 1))
		       initially (moxie-set-wb-ack-i *cpu* 0)
		       do (progn (tick-down) (tick-up)))
		    ;; Have we waited too long?
		    (if (> count 10000) (finish-loop))
		    ;; Insert some number of wait cycles to fetch from memory...
		    (loop for wait from 1 to memory-wait-cycles
		       initially (moxie-set-wb-ack-i *cpu* 0)
		       do (progn (tick-down) (tick-up)))
		    ;; Return NOP instruction...
		    (moxie-set-wb-dat-i *cpu* 15)
		    (moxie-set-wb-ack-i *cpu* 1)
		    (tick-down))
		;; Did we end up at the right $PC?
		finally (is (= (moxie-get-wb-adr-o *cpu*) 8192))))))

;;; ---------------------------------------------------------------------------
;;; Load an ELF executable and run it.
;;; ---------------------------------------------------------------------------
(defun sim-read-byte (mem adr)
  (let ((page (gethash (logand adr #xFFFFF000) mem)))
    (if page
	(elt page (logand adr #x00000FFF))
	0)))

(defun sim-write-byte (mem adr val)
  (let ((page (gethash (logand adr #xFFFFF000) mem))
	(offset (logand adr #x00000FFF)))
    (if page
	(setf (elt page offset) val)
	(let ((page (make-array #x1000 :initial-element 0)))
	  (setf (elt page offset) val)
	  (setf (gethash (logand adr #xFFFFF000) mem) page)))))

;;; Is this something we read from the ELF file and insert in memory?
(defun elf-section-allocatable-p (sec)
  (eq (logand (elf:flags (elf:sh sec)) 2) 2))

(defun load-elf-executable (mem filename)
  (let ((exe (elf:read-elf filename)))
    (mapc (lambda (sec)
	    (if (elf-section-allocatable-p sec)
		(if (eq (elf:type sec) :PROGBITS)
		    (let ((adr (elf:address (elf:sh sec))))
		      (loop for byte being the elements of (elf:data sec)
			 do (progn
			      (sim-write-byte mem adr byte)
			      (setq adr (+ adr 1))))))))
	  (elf:sections exe))))

(defun load-and-run (filename)
      (let ((mem (make-hash-table)))
	(load-elf-executable mem filename)
	(reset-cycles 10)
	(loop for count from 1 to 99999
	   do
	     (progn
	       ;; Wait until there is a bus request...
	       (tick-up)
	       (loop until (and (eq (moxie-get-wb-stb-o *cpu*) 1)
				(eq (moxie-get-wb-cyc-o *cpu*) 1))
		  initially (moxie-set-wb-ack-i *cpu* 0)
		  do (progn (tick-down) (format t ".") (tick-up)))
	       ;; Read or write...
	       (let ((adr (moxie-get-wb-adr-o *cpu*))
		     (sel (moxie-get-wb-sel-o *cpu*)))
		 (format t "** ~A **~%" (moxie-get-wb-we-o *cpu*))
		 (if (eq (moxie-get-wb-we-o *cpu*) 1)
		     ;; We are writing.  Test for our magic exit code
		     ;; to end the simulation.
		     (let ((value (moxie-get-wb-dat-o *cpu*)))
		       (format t "W@~X[~A]: ~X~%" adr (moxie-get-wb-sel-o *cpu*) value)
		       (cond
			 ((eq sel 1)
			  (sim-write-byte mem adr (ldb (byte 8 0) value)))	
			 ((eq sel 3)
			  (sim-write-byte mem adr (ldb (byte 8 8) value))
			  (sim-write-byte mem (+ adr 1) (ldb (byte 8 0) value)))
			 ((eq sel 15)
			  (sim-write-byte mem adr (ldb (byte 8 24) value))
			  (sim-write-byte mem (+ adr 1) (ldb (byte 8 16) value))
			  (sim-write-byte mem (+ adr 2) (ldb (byte 8 8) value))
			  (sim-write-byte mem (+ adr 3) (ldb (byte 8 0) value)))
			 (t
			  (format t "ERROR: INVALID sel~%")))
		       (if (eq adr #x00C0FFEE)
			   (return)))
		     ;; We are reading
		     (let* ((low-byte (sim-read-byte mem adr))
			    (high-byte (sim-read-byte mem (+ adr 1))))
		       (format t "R: @~X~%" adr)
		       (moxie-set-wb-dat-i *cpu* (+ (* low-byte 256) high-byte)))))
	       (moxie-set-wb-ack-i *cpu* 1)
	       (tick-down)))
	;; Did we end up at the right $PC?
	(format t "C0FFEE: ~X ~X ~X ~X~%"
		(sim-read-byte mem #x00c0ffee)
		(sim-read-byte mem #x00c0ffef)
		(sim-read-byte mem #x00c0fff0)
		(sim-read-byte mem #x00c0fff1))
	(is (= (sim-read-byte mem #x00c0ffee) #xf) (namestring filename))))

(test run-executable-tests
      (let ((test-binaries (directory "bin/*.x")))
	(mapc #'load-and-run test-binaries)))

; (run! 'check-boot-address)
; (run! 'run-nop-sequence)
(run! 'run-executable-tests)


(exit)
