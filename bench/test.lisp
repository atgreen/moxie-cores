;;; This test program uses a verilator simulator of the moxie
;;; instruction cache module, wrapped in a thin lisp veneer by
;;; wrapilator.

(load "obj_dir/verilated-icache.lisp")
(use-package :verilated-icache)

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

(defun test-sequential-read (start-address stop-address step)

  (loop for address from start-address to stop-address by step do
       
     ;; set the address we're looking for and strobe the cache
       (icache-set-adr-i *ic* address) 
       (icache-set-stb-i *ic* 1)
       
       (loop do
	    (tick-up)

	  ;; simulate wishbone main memory
	    (let ((check-mem (and (icache-get-wb-stb-o *ic*)
				  (icache-get-wb-cyc-o *ic*))))
	      (icache-set-wb-ack-i *ic* check-mem)
	      (if (eq 1 check-mem)
		  (icache-set-wb-dat-i *ic* (icache-get-wb-adr-o *ic*))))
	    
	    (tick-down)

	  ;; check for a cache hit
	    (if (eq 1 (icache-get-hit-o *ic*))
		(let ((memory-value (icache-get-inst-o *ic*)))
		  (format t "HIT @ 0x~A = 0x~A ~%" address memory-value)
		  (if (not (eq address memory-value))
		      (format t "ERROR: CACHE FAILURE *********************~%"))
		  (return))
		(format t "fill 0x~A~%" (icache-get-wb-adr-o *ic*))))))


(defun runtest ()
  
  ;; Zero all inputs and hold reset for a few cycles
  (icache-set-stb-i *ic* 0)
  (icache-set-wb-ack-i *ic* 0)
  (icache-set-wb-dat-i *ic* 0)
  (icache-set-adr-i *ic* 0)
  (reset-cycles 10)
  
  (loop for i from 0 to 3 do
       (reset-cycles 10)
       (loop for j from 0 to 3 do
	    (test-sequential-read #x1000 #x1110 2))))

(runtest)
(exit)
