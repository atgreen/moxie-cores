(load "quicklisp.lisp")

(quicklisp-quickstart:install :path "/root/quicklisp/")

;;; Load some things we will probably need
(ql:quickload :FiveAM)
(ql:quickload :cffi)
(ql:quickload :elf)

(with-open-file (out "/root/.sbclrc" :direction :output)
  (format out "(load \"/root/quicklisp\/setup.lisp\")"))
