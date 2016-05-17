;;;; package.lisp

(defpackage #:cl-bench
  (:use #:cl)
  (:import-from #:alexandria #:ensure-list)
  (:export #:bench-run-1
           #:bench-run
           #:defbench))
