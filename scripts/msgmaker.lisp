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

;;; Copyright (C) 2012  Anthony Green <green@moxielogic.com>
;;; Distrubuted under the terms of the GPL v3 or later.

;;; This program reads an input file called messages.org, which
;;; contains an emacs org-mode table with ASCII message text.
;;; It parses this table and produces a ROM file message.bin
;;; as well as a corresponding verilog source file with 
;;; message offsets.
;;;

;;; Thanks for Zach Beane for quicklisp (which you'll need to install)
;;;

(ql:quickload "cl-ppcre")

(defpackage :msgmaker
  (:use :cl :cl-user :cl-ppcre))

(in-package :msgmaker)

(let ((in (open "messages.org" :if-does-not-exist nil))
      (index 1))
  (when in
    ;; skip to table contents
    (loop for filepos = (file-position in)
	  for line = (read-line in nil)
 	  until (let ((s (cl-ppcre:split "\\|" line)))
		  (equal (length s) 3)))
    (read-line in nil)

    ;; We're at the table contents now.  Parse it and write our new
    ;; messages.bin and messages.vh files.
    (with-open-file
     (vout "messages.vh" :direction :output
	   :if-exists :supersede)
     (with-open-file 
      (bout "messages.bin" :direction :output
	    :if-exists :supersede)
      (format vout "`define MSG_OFFSET_EMPTY 0~%")
      (format bout "@00000000~%00")
      (loop for filepos = (file-position in)
	    for line = (read-line in nil)
	    while line do 
	    (let ((s (cl-ppcre:split "\\|" line)))
	      (if (equal 3 (length s))
		  (destructuring-bind (junk1 name msg &rest junk2) 
		      (mapcar (lambda (v) (string-trim " " v)) s)
		    (format vout "`define MSG_OFFSET_~A ~A~%" name index)
		    (loop for char across msg do
			  (incf index)
			  (format bout "~X " (char-code char)))
		    (format bout "00")))))
      (format bout "~%")))
    (close in)))
    
(sb-ext:quit)

