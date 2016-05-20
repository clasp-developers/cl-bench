;;;; cl-bench.asd

(asdf:defsystem #:cl-bench
  :description "Common Lisp implementation benchmarking"
  :author "Eric Marsden"
  :maintainer "Daniel 'jackdaniel' Kochmański"
  :license "Public Domain"
  :depends-on (#:alexandria #:trivial-garbage)
  :serial t
  :components ((:file "package")
               (:file "cl-bench")
               (:file "support")
               (:module "files"
                :components
                ((:file "arrays")
                 (:file "bignum")
                 (:file "boehm-gc")
                 (:file "clos")
                 (:file "crc40")
                 (:file "deflate")
                 (:file "gabriel")
                 (:file "hash")
                 (:file "math")
                 (:file "ratios")
                 (:file "richards")
                 (:file "misc")))
               (:file "tests")))

(asdf:defsystem #:cl-bench/report
  :description "cl-bench reporting facilities"
  :author "Eric Marsden"
  :maintainer "Daniel 'jackdaniel' Kochmański"
  :license "Public Domain"
  :depends-on (#:cl-bench #:cl-who)
  :serial t
  :components ((:module "report"
                :components
                ((:file "report")
                 (:file "graph-report")
                 (:file "html-report")))))
