;;; ucoder.lisp - build moxbit's microcode ROMs from microcode.org
;;;
;;; Copyright (C) 2026  Anthony Green <green@moxielogic.com>
;;; Distributed under the terms of the GPL v3 or later.
;;;
;;; Usage: sbcl --script ucoder.lisp
;;;
;;; Reads the fields, ucode and dispatch tables in microcode.org and
;;; writes moxbit_ucode.v (the two ROMs) and moxbit_ucode.vh (the
;;; field positions, value codes and µop addresses moxbit.v uses).

(defun trim (s) (string-trim '(#\Space #\Tab) s))

(defun split (s ch)
  (loop with start = 0
        for pos = (position ch s :start start)
        collect (subseq s start pos)
        while pos do (setf start (1+ pos))))

(defun words (s)
  (remove "" (split (substitute #\Space #\Tab (trim s)) #\Space) :test #'string=))

(defun fail (fmt &rest args)
  (format *error-output* "ucoder: ~?~%" fmt args)
  (sb-ext:exit :code 1))

(defun read-tables (file)
  "An alist of (name . rows) for each #+NAME'd table; a row is a list
of cells, and the first row is the header."
  (with-open-file (in file)
    (loop with tables = nil and name = nil and rows = nil
          for line = (read-line in nil)
          for tl = (and line (trim line))
          do (cond ((and tl (> (length tl) 0) (char= (char tl 0) #\|))
                    (unless (and (> (length tl) 1) (char= (char tl 1) #\-))
                      (push (mapcar #'trim (butlast (rest (split tl #\|)))) rows)))
                   (t
                    (when (and name rows)
                      (push (cons name (reverse rows)) tables))
                    (setf rows nil)
                    (setf name (and tl (> (length tl) 8)
                                    (string-equal "#+NAME:" (subseq tl 0 7))
                                    (trim (subseq tl 7))))))
          while line
          finally (return tables))))

(defun table (tables name)
  (or (cdr (assoc name tables :test #'string=)) (fail "no table ~A" name)))

;;; --- Fields ----------------------------------------------------------

(defstruct field name width values lsb)

(defun parse-fields (rows)
  (let ((fields (loop for (name width values) in (rest rows)
                      collect (make-field :name name
                                          :width (parse-integer width)
                                          :values (words values))))
        (lsb 0))
    ;; The first field listed is the most significant.
    (dolist (f (reverse fields) fields)
      (setf (field-lsb f) lsb)
      (incf lsb (field-width f)))))

(defun word-width (fields)
  (reduce #'+ fields :key #'field-width))

;;; A Verilog name for a value: kind "pass" is KIND_PASS, "-" is NONE,
;;; "+2" is P2 and "0" is ZERO.
(defun value-name (field value)
  (format nil "~:@(~A_~A~)" (field-name field)
          (cond ((string= value "-") "none")
                ((string= value "0") "zero")
                (t (substitute #\P #\+ value)))))

;;; --- Microcode -------------------------------------------------------

(defun cell (header row name)
  (let ((i (position name header :test #'string=)))
    (if i (or (nth i row) "") (fail "no column ~A" name))))

(defun cond-target (c)
  (let ((colon (position #\: c)))
    (and colon (subseq c (1+ colon)))))

(defun layout (rows)
  "Give each µop an address.  The fetch µop is at zero, and a µop that
is the not-taken side of a condition is put at an even address, its
taken side at the next one."
  (let* ((header (first rows))
         (uops (rest rows))
         (evens (loop for r in uops
                      for c = (cell header r "cond")
                      when (cond-target c) collect (cell header r "next")))
         (addresses (make-hash-table :test #'equal))
         (placed nil)
         (addr 0))
    (unless (string= (cell header (first uops) "label") "fetch")
      (fail "the first µop has to be fetch"))
    (dolist (r uops)
      (let ((label (cell header r "label")))
        (when (and (member label evens :test #'string=) (oddp addr))
          (push nil placed)               ; padding
          (incf addr))
        (when (gethash label addresses)
          (fail "µop ~A is defined twice" label))
        (setf (gethash label addresses) addr)
        (push r placed)
        (incf addr)))
    (when (> addr 255)
      (fail "~D µops do not fit in the ROM" addr))
    (values (reverse placed) addresses header)))

(defun label-address (addresses label)
  (cond ((string= label "fetch") 0)
        ((string= label "disp2") 255)
        (t (or (gethash label addresses) (fail "no µop ~A" label)))))

(defun encode-value (field value addresses)
  (let ((values (field-values field)))
    (cond ((string= (first values) "#signed")
           (let ((n (if (string= value "") 0 (parse-integer value))))
             (unless (< (- (ash 1 (1- (field-width field)))) n (ash 1 (1- (field-width field))))
               (fail "~D does not fit in ~A" n (field-name field)))
             (ldb (byte (field-width field) 0) n)))
          ((string= (first values) "#label")
           (label-address addresses value))
          ((string= value "") 0)
          (t (or (position value values :test #'string=)
                 (fail "~A is not a value of ~A" value (field-name field)))))))

(defun encode-uop (fields header row addresses)
  (let ((word 0)
        (label (cell header row "label")))
    (dolist (f fields word)
      (let* ((name (field-name f))
             (text (cell header row name))
             (code (cond ((string= name "cond")
                          (let ((c (subseq text 0 (position #\: text))))
                            (encode-value f c addresses)))
                         ((string= name "next")
                          (if (string= (cell header row "kind") "fetch")
                              0
                              (encode-value f text addresses)))
                         (t (encode-value f text addresses)))))
        (when (string= name "cond")
          (let ((taken (cond-target text)))
            (when taken
              (let ((next (label-address addresses (cell header row "next"))))
                (unless (= (label-address addresses taken) (1+ next))
                  (fail "in ~A, ~A has to follow ~A" label taken (cell header row "next")))))))
        (setf word (logior word (ash code (field-lsb f))))))))

(defun check-bus-spacing (placed header)
  "Two bus µops in a row would put two requests back to back."
  (let ((kinds (make-hash-table :test #'equal)))
    (dolist (r placed)
      (when r (setf (gethash (cell header r "label") kinds) (cell header r "kind"))))
    (dolist (r placed)
      (when r
        (let ((kind (cell header r "kind"))
              (next (gethash (cell header r "next") kinds)))
          (when (and (member kind '("read" "readb" "write" "writeb") :test #'string=)
                     (member next '("read" "readb" "write" "writeb" "fetch") :test #'string=))
            (fail "~A is a bus µop followed by another" (cell header r "label"))))))))

;;; --- Dispatch --------------------------------------------------------

(defun parse-ops (text)
  (loop for w in (words text)
        for dash = (position #\- w)
        append (if dash
                   (loop for i from (parse-integer w :end dash :radix 16)
                           to (parse-integer w :start (1+ dash) :radix 16)
                         collect i)
                   (list (parse-integer w :radix 16)))))

(defun dispatch (rows addresses mdu)
  "A vector of 512 µop addresses: the head for each opcode, then
where disp2 goes for each."
  (let* ((header (first rows))
         (undefined (label-address addresses "e_undef"))
         (table (make-array 512 :initial-element undefined)))
    (dolist (r (rest rows) table)
      (unless (and (string= (cell header r "mdu") "y") (not mdu))
        (dolist (op (parse-ops (cell header r "ops")))
          (setf (aref table op) (label-address addresses (cell header r "head")))
          (let ((then (cell header r "then")))
            (unless (string= then "")
              (setf (aref table (+ 256 op)) (label-address addresses then)))))))))

;;; --- Output ----------------------------------------------------------

(defun main ()
  (let* ((tables (read-tables "microcode.org"))
         (fields (parse-fields (table tables "fields")))
         (width (word-width fields)))
    (multiple-value-bind (placed addresses header) (layout (table tables "ucode"))
      (check-bus-spacing placed header)
      (let ((with-mdu (dispatch (table tables "dispatch") addresses t))
            (without-mdu (dispatch (table tables "dispatch") addresses nil)))
        (with-open-file (out "moxbit_ucode.v" :direction :output :if-exists :supersede)
          (format out "// moxbit_ucode.v - moxbit's microcode and dispatch ROMs~%//~%")
          (format out "// Generated from microcode.org by ucoder.lisp.  Do not edit.~%~%")
          (format out "module moxbit_ucode #(parameter MDU = 1) (~%")
          (format out "  input             clk_i,~%")
          (format out "  input [7:0]       uaddr_i,~%")
          (format out "  output reg [~D:0] uword_o,~%" (1- width))
          (format out "  input [8:0]       daddr_i,~%")
          (format out "  output reg [7:0]  disp_o~%  );~%~%")
          ;; Block RAM on every FPGA, not logic: Yosys builds ROMs this
          ;; size out of LUTs for Xilinx otherwise.
          (format out "  (* rom_style = \"block\" *)~%  reg [~D:0] urom [0:255];~%" (1- width))
          (format out "  (* rom_style = \"block\" *)~%  reg [7:0] drom [0:511];~%~%")
          (format out "  integer i;~%  initial~%    begin~%")
          (format out "      for (i = 0; i < 256; i = i + 1)~%	urom[i] = 0;~%")
          (loop for r in placed
                for addr from 0
                when r do (format out "      urom[~D] = ~D'h~X;	// ~A~%" addr width
                                  (encode-uop fields header r addresses)
                                  (cell header r "label")))
          (loop for i from 0 below 512
                for a = (aref with-mdu i)
                for b = (aref without-mdu i)
                do (if (= a b)
                       (format out "      drom[~D] = 8'd~D;~%" i a)
                       (format out "      drom[~D] = MDU ? 8'd~D : 8'd~D;~%" i a b)))
          (format out "    end~%~%")
          (format out "  always @(posedge clk_i)~%    begin~%")
          (format out "      uword_o <= urom[uaddr_i];~%")
          (format out "      disp_o <= drom[daddr_i];~%    end~%~%")
          (format out "endmodule // moxbit_ucode~%"))
        (with-open-file (out "moxbit_ucode.vh" :direction :output :if-exists :supersede)
          (format out "// moxbit_ucode.vh - the layout of moxbit's µop word~%//~%")
          (format out "// Generated from microcode.org by ucoder.lisp.  Do not edit.~%~%")
          (format out "/* verilator lint_off UNUSEDPARAM */~%")
          (format out "localparam UWIDTH = ~D;~%~%" width)
          (dolist (f fields)
            (format out "localparam U_~:@(~A~)_LSB = ~D, U_~:@(~A~)_W = ~D;~%"
                    (field-name f) (field-lsb f) (field-name f) (field-width f)))
          (terpri out)
          (dolist (f fields)
            (unless (char= (char (first (field-values f)) 0) #\#)
              (loop for v in (field-values f)
                    for code from 0
                    do (format out "localparam [~D:0] ~A = ~D;~%"
                               (1- (field-width f)) (value-name f v) code))))
          (terpri out)
          (format out "localparam [7:0] UA_FETCH = 8'd0;~%")
          (format out "localparam [7:0] UA_DISP2 = 8'd255;~%")
          (format out "localparam [7:0] UA_E_IRQ = 8'd~D;~%"
                  (label-address addresses "e_irq"))
          (format out "/* verilator lint_on UNUSEDPARAM */~%"))))))

(main)
