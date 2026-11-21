;;; zephyr.lisp - run a Zephyr image on the verilated mox125 core.
;;;
;;; Copyright (C) 2026  Anthony Green <green@moxielogic.com>
;;; Distributed under the terms of the GPL v3 or later.

;;; Usage: sbcl --non-interactive --load zephyr.lisp <zephyr.elf> [max-cycles [trace-from]]
;;;
;;; This is the "mox125_sim" board the Zephyr port targets.  Besides
;;; RAM it provides, on the wishbone bus:
;;;
;;;   0xF0000000  console      a byte written here is printed
;;;   0xF0000020  pic pending  one bit per interrupt source (read)
;;;   0xF0000024  pic enable   which sources may interrupt
;;;   0xF0000030  timer ctrl   write 1 to start
;;;   0xF0000034  timer period clocks between interrupts
;;;   0xF0000038  timer ack    write to drop the timer interrupt
;;;   0xF000003C  timer count  clocks since reset
;;;   0xF00000F0  exit         a write ends the simulation
;;;
;;; Registers are 32 bits wide and big-endian, so on the 16-bit bus the
;;; low half is at offset 2.  The timer is interrupt source 0.
;;;
;;; The run also ends when the console prints a line containing
;;; "PROJECT EXECUTION", which is how Zephyr test suites report.
;;; With a third argument, everything from that clock on is dumped to
;;; zephyr.vcd.

(asdf:load-system :elf)
(setq elf:*endian* :big)

(load "obj_dir/verilated-moxie.lisp")
(use-package :verilated-moxie)

(defconstant +ram-base+ #x1000)
(defconstant +ram-size+ #x100000)

(defvar *ram* (make-array +ram-size+ :element-type '(unsigned-byte 8)
                                     :initial-element 0))
(defvar *cycle* 0)
(defvar *pic-pending* 0)
(defvar *pic-enable* 0)
(defvar *timer-running* nil)
(defvar *timer-period* 0)
(defvar *timer-next* 0)
(defvar *count-latch* 0)
(defvar *done* nil)
(defvar *last-output* 0)
(defvar *line* (make-array 0 :element-type 'character :adjustable t :fill-pointer t))

(defun ram-p (adr)
  (and (>= adr +ram-base+) (< adr (+ +ram-base+ +ram-size+ -1))))

(defun ram-read16 (adr)
  (let ((i (- adr +ram-base+)))
    (logior (ash (aref *ram* i) 8) (aref *ram* (1+ i)))))

(defun load-elf-executable (filename)
  (dolist (sec (elf:sections (elf:read-elf filename)))
    ;; Anything allocatable with contents goes into memory.
    ;; The elf library hands back a keyword when exactly one flag is set.
    (when (and (let ((flags (elf:flags (elf:sh sec))))
                 (if (integerp flags)
                     (logtest flags 2)
                     (eq flags :allocatable)))
               (eq (elf:type sec) :progbits))
      (let ((adr (elf:address (elf:sh sec))))
        (loop for byte being the elements of (elf:data sec)
              do (setf (aref *ram* (- adr +ram-base+)) byte)
                 (incf adr))))))

(defun bus-read (adr)
  (cond
    ((ram-p adr) (ram-read16 adr))
    ((= adr #xF0000022) (logand *pic-pending* #xFFFF))
    ((= adr #xF0000026) (logand *pic-enable* #xFFFF))
    ;; Reading the high half of the count latches the low half.
    ((= adr #xF000003C) (setf *count-latch* *cycle*)
     (ldb (byte 16 16) *count-latch*))
    ((= adr #xF000003E) (ldb (byte 16 0) *count-latch*))
    ((>= adr #xF0000000) 0)
    ;; Nothing lives anywhere else, so the program has gone astray.
    (t (format t "~&[read of unmapped address ~X]~%" adr)
       (setf *done* t)
       0)))

(defun console-write (char)
  (write-char char)
  (finish-output)
  (setf *last-output* *cycle*)
  ;; Zephyr's test suites finish with a verdict line; stop there.
  (if (char= char #\Newline)
      (progn
        (when (search "PROJECT EXECUTION" *line*)
          (setf *done* t))
        (setf (fill-pointer *line*) 0))
      (vector-push-extend char *line*)))

;;; Debugging aids, set from the environment:
;;;   MOX_WATCH=lo-hi    report every write to that address range (hex)
;;;   MOX_DUMP=a,b,...   print the 32-bit words at those addresses (hex) on exit
(defun parse-hex-list (string separator)
  (when string
    (loop for start = 0 then (1+ end)
          for end = (position separator string :start start)
          collect (parse-integer string :start start :end end :radix 16)
          while end)))

(defvar *watch* (parse-hex-list (sb-ext:posix-getenv "MOX_WATCH") #\-))
(defvar *dump* (parse-hex-list (sb-ext:posix-getenv "MOX_DUMP") #\,))

(defun bus-write (adr sel value)
  (when (and *watch* (<= (first *watch*) adr) (< adr (second *watch*)))
    (format t "~&[clock ~:D: write ~4,'0X to ~X]~%" *cycle* value adr))
  (cond
    ((ram-p adr)
     (let ((i (- adr +ram-base+)))
       (if (= sel 1)
           (setf (aref *ram* i) (ldb (byte 8 0) value))
           (setf (aref *ram* i) (ldb (byte 8 8) value)
                 (aref *ram* (1+ i)) (ldb (byte 8 0) value)))))
    ((= adr #xF0000000) (console-write (code-char (ldb (byte 8 0) value))))
    ((= adr #xF0000026) (setf *pic-enable* value))
    ((= adr #xF0000032)
     (setf *timer-running* (not (zerop value))
           *timer-next* (+ *cycle* *timer-period*)))
    ((= adr #xF0000034) (setf (ldb (byte 16 16) *timer-period*) value))
    ((= adr #xF0000036) (setf (ldb (byte 16 0) *timer-period*) value))
    ((= adr #xF000003A) (setf *pic-pending* (logandc2 *pic-pending* 1)))
    ((= adr #xF00000F2) (setf *done* t))))

(defun run (filename max-cycles trace-from)
  (load-elf-executable filename)
  (when trace-from
    (traceEverOn 1))
  (let ((cpu (moxie-new))
        (trace nil)
        ;; The request we are in the middle of answering, if any.
        (pending-adr nil)
        (pending-we nil))
    (when trace-from
      (setf trace (VerilatedVcdC-new))
      (moxie-trace cpu trace 99)
      (VerilatedVcdC-open trace "zephyr.vcd"))
    (moxie-set-irq-i cpu 0)
    (moxie-set-wb-ack-i cpu 0)
    (moxie-set-rst-i cpu 1)
    (loop repeat 10
          do (moxie-set-clk-i cpu 1) (moxie-eval cpu)
             (moxie-set-clk-i cpu 0) (moxie-eval cpu))
    (moxie-set-rst-i cpu 0)
    (loop until (or *done* (>= *cycle* max-cycles))
          do (moxie-set-clk-i cpu 1)
             (moxie-eval cpu)
             ;; Behave like the SoC's slaves: act on a request when it
             ;; first appears and ack it on the following clock.
             (if (and (eq (moxie-get-wb-stb-o cpu) 1)
                      (eq (moxie-get-wb-cyc-o cpu) 1))
                 (let ((adr (moxie-get-wb-adr-o cpu))
                       (we (moxie-get-wb-we-o cpu)))
                   (if (and (eql adr pending-adr) (eql we pending-we))
                       (progn
                         (moxie-set-wb-ack-i cpu 1)
                         (setf pending-adr nil))
                       (progn
                         (moxie-set-wb-ack-i cpu 0)
                         (setf pending-adr adr pending-we we)
                         (if (eq we 1)
                             (bus-write adr (moxie-get-wb-sel-o cpu)
                                        (moxie-get-wb-dat-o cpu))
                             (moxie-set-wb-dat-i cpu (bus-read adr))))))
                 (progn
                   (moxie-set-wb-ack-i cpu 0)
                   (setf pending-adr nil)))
             (when (and *timer-running* (>= *cycle* *timer-next*))
               (setf *pic-pending* (logior *pic-pending* 1))
               (incf *timer-next* *timer-period*))
             (moxie-set-irq-i cpu (if (zerop (logand *pic-pending* *pic-enable*)) 0 1))
             (when (and trace (>= *cycle* trace-from))
               (VerilatedVcdC-dump trace (* 2 *cycle*)))
             (moxie-set-clk-i cpu 0)
             (moxie-eval cpu)
             (when (and trace (>= *cycle* trace-from))
               (VerilatedVcdC-dump trace (1+ (* 2 *cycle*))))
             (incf *cycle*))
    (when trace
      (VerilatedVcdC-close trace))
    (dolist (adr *dump*)
      (format t "~&[~X: ~4,'0X~4,'0X]~%" adr (ram-read16 adr) (ram-read16 (+ adr 2))))
    (format t "~&[~A after ~:D clocks, last output at ~:D]~%"
            (if *done* "exit requested" "stopped") *cycle* *last-output*)))

(let ((args (cdr sb-ext:*posix-argv*)))
  (unless args
    (format t "usage: sbcl --non-interactive --load zephyr.lisp <zephyr.elf> [max-cycles [trace-from]]~%")
    (sb-ext:exit :code 2))
  (run (first args)
       (if (second args) (parse-integer (second args)) 2000000)
       (if (third args) (parse-integer (third args)))))

(sb-ext:exit)
