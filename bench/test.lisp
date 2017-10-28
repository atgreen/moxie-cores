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
(load "obj_dir/verilated-icache.lisp")
(use-package :verilated-icache)
(use-package :it.bese.FiveAM)

(setq *test-dribble* t)
(setq *print-names* t)

;;; Create a new icache
(defvar *ic* (icache-new))

;;; Clock up and eval
(defun tick-up ()
  (icache-set-clk-i *ic* 1)
  (icache-eval *ic*))

;;; Clock down and eval
(defun tick-down ()
  (icache-set-clk-i *ic* 0)
  (icache-eval *ic*))

;;; Hold reset high for CYCLES
(defun reset-cycles (cycles)
  (icache-set-rst-i *ic* 1)
  (loop for i from 1 to cycles do
       (tick-up)
       (tick-down))
  (icache-set-rst-i *ic* 0))

(defun random-from-range (start end)
  (logand (+ start (random (+ 1 (- end start))))
	  (lognot 1)))

(defun test-random-read (start-address stop-address count)
  (loop for i from 0 to count do

       (let ((address (random-from-range start-address stop-address)))
	 (icache-set-adr-i *ic* address) 
	 (icache-set-stb-i *ic* 1)
       
	 (loop
	    do (tick-up)
	      
	    ;; simulate wishbone main memory
	      (let ((check-mem (and (icache-get-wb-stb-o *ic*)
				    (icache-get-wb-cyc-o *ic*)))
		    (wb-fetch-address (icache-get-wb-adr-o *ic*)))
	  
		(icache-set-wb-ack-i *ic* check-mem)
		(if (eq 1 check-mem)
		    (icache-set-wb-dat-i *ic* wb-fetch-address))
	  
		(tick-down)
	  
		;; check for a cache hit
		(if (eq 1 (icache-get-hit-o *ic*))
		    (let ((memory-value (icache-get-inst-o *ic*)))
		      (is (eq address memory-value))
		      (return))))))))

(defun test-sequential-read (start-address stop-address step)

  (loop for address from start-address to stop-address by step do
       
     ;; set the address we're looking for and strobe the cache
       (icache-set-adr-i *ic* address) 
       (icache-set-stb-i *ic* 1)
       
       (loop
	do (tick-up)

	  ;; simulate wishbone main memory
	(let ((check-mem (and (icache-get-wb-stb-o *ic*)
			      (icache-get-wb-cyc-o *ic*)))
	      (wb-fetch-address (icache-get-wb-adr-o *ic*)))
	  
	  (icache-set-wb-ack-i *ic* check-mem)
	  (if (eq 1 check-mem)
	      (icache-set-wb-dat-i *ic* wb-fetch-address))
	  
	  (tick-down)
	  
	  ;; check for a cache hit
	  (if (eq 1 (icache-get-hit-o *ic*))
	      (let ((memory-value (icache-get-inst-o *ic*)))
		(is (eq address memory-value))
		(return)))))))

(icache-set-stb-i *ic* 0)
(icache-set-wb-ack-i *ic* 0)
(icache-set-wb-dat-i *ic* 0)
(icache-set-adr-i *ic* 0)

(test sequential-read
      (loop for i from 0 to 3 do
	   (reset-cycles 10)
	   (loop for j from 0 to 3 do
		(test-sequential-read 1000 10000 4))))

(test random-read
      (test-random-read 4128 50128 50000))

(explain! (run 'random-read))
(run! 'sequential-read)

(exit)
