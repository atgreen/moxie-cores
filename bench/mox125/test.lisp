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

(moxie-set-wb-ack-i *cpu* 0)
(moxie-set-wb-dat-i *cpu* 0)

;;; after reset, the core should start reading from 0x1000
(test boot
      (reset-cycles 10)
      (loop until (and (eq (moxie-get-wb-stb-o *cpu*) 1)
		       (eq (moxie-get-wb-cyc-o *cpu*) 1))
	 do (progn
	      (tick-up)
	      (tick-down)))
      (is (= (moxie-get-wb-adr-o *cpu*) 4096)))

(run! 'boot)

(exit)
