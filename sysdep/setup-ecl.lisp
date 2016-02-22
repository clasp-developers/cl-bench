;;; setup file for cl-bench running in ECL
;;
;; tested with ECL 0.9
;;
;; see <URL:http://ecls.sf.net/>

(load "defpackage")
(in-package :cl-bench)

(ext:set-limit 'ext:c-stack (* 8 1024 1024))

(defun bench-gc ()
  (si:gc t))

#+threads
(defmacro with-spawned-thread (&body body)
  `(mp:process-run-function nil #'(lambda () ,@body)))

#-threads
(defmacro with-spawned-thread (&body body)
  `(progn ,@body))

;; to autoload the compiler
(compile 'bench-gc)

#-ecl-bytecmp
(setq c::*cc-flags* (concatenate 'string "-I. " c::*cc-flags*))

;; EOF
