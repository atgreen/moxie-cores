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

;;; Copyright (C) 2012, 2015  Anthony Green <green@moxielogic.com>
;;; Distrubuted under the terms of the GPL v3 or later.

;;; This program reads an input file called microcode.org, which
;;; contains an emacs org-mode table with moxie microcode information.
;;; It parses this table and produces a microcode ROM file
;;; microcode.bin, suitable for inclusion in the moxie core RTL.
;;;

;;; Thanks for Zach Beane for quicklisp (which you'll need to install)
;;;

(ql:quickload "cl-ppcre")

(defpackage :microcoder
  (:use :cl :cl-user :cl-ppcre))

(in-package :microcoder)

(let ((in (open "microcode.org" :if-does-not-exist nil))
      (opcode-array (make-array 64 :initial-element nil)))
  (when in
    ;; skip to table contents
    (loop for filepos = (file-position in)
	  for line = (read-line in nil)
 	  until (let ((s (cl-ppcre:split "\\|" line)))
		  (equal (length s) 10)))
    (read-line in nil)

    ;; We're at the table contents now.  Parse it and write our new
    ;; microcode.bin file.
    (with-open-file 
     (out "microcode.bin" :direction :output
	  :if-exists :supersede)
     (loop for filepos = (file-position in)
	   for line = (read-line in nil)
	   while line do 
	   (let ((s (cl-ppcre:split "\\|" line)))
	     (if (equal 10 (length s))
		 (destructuring-bind (junk1 name code wA? wB? rA? rB? rm? wm? &rest junk2) 
				     (mapcar (lambda (v) (string-trim " " v)) s)
				     (setf (aref opcode-array (parse-integer code :radix 2))
					   (list name wA? wB? rA? rB? rm? wm?))))))
     (loop for i from 0 to 63
	   do (let ((o (aref opcode-array i)))
		(if o
		    (mapc (lambda (v)
			    (let ((n (parse-integer v :radix 2)))
			      (cond 
			       ((equal n 0) (format out "0"))
			       ((equal n 1) (format out "1"))
			       (t (error "bad table entry")))))
			  (cdr o))
		  (format out "000000"))
		(format out "~%"))))
  (close in)))
    
(sb-ext:quit)

