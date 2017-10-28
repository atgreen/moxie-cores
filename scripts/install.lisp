(load "quicklisp.lisp")

(quicklisp-quickstart:install :path "/root/quicklisp/")

(with-open-file (out "/root/.sbclrc" :direction :output)
  (format out "(load \"/root/quicklisp\/setup.lisp\")"))
