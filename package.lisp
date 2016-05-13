;;;; package.lisp

(defpackage #:cl-bench
  (:use #:cl)
  (:export #:bench-run
           #:defbench))
