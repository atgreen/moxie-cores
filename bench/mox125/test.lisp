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

;;; This test program uses a verilator simulator of the moxie
;;; instruction cache module, wrapped in a thin lisp veneer by
;;; wrapilator.

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
(test boot
      (moxie-set-wb-ack-i *cpu* 0)
      (reset-cycles 10)
      (loop until (and (eq (moxie-get-wb-stb-o *cpu*) 1)
		       (eq (moxie-get-wb-cyc-o *cpu*) 1))
	 do (progn
	      (tick-up)
	      (tick-down)))
      (is (= (moxie-get-wb-adr-o *cpu*) 4096)))

;;; ---------------------------------------------------------------------------
;;; Execute 4k of NOP instructions, making sure we end up at the expceted $PC.
;;; Don't execute more than a large number of instructions.
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
		       do (progn (moxie-set-wb-ack-i *cpu* 0) (tick-down) (tick-up)))
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
		;; Did we end up at the righ $PC?
		finally (is (= (moxie-get-wb-adr-o *cpu*) 8192))))))

(defvar *elf* (elf:read-elf "bootrom.x"))

(run! 'boot)
(run! 'run-nop-sequence)

(exit)
