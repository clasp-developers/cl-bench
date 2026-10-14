;; hashtable and READ-LINE benchmarking code
;;
;; some code by Paul Foley
;; Time-stamp: <2016-05-10 13:11:57 jack>

(defpackage :cl-bench.hash
  (:use :common-lisp)
  (:export #:run-slurp-lines
           #:setup-string
           #:setup-fixnum
           #:setup-mixbag
           #:bench-htable
           #:bench-setrem
           #:bench-sxhash))

(in-package :cl-bench.hash)


(defun read-many-lines (file)
  (with-open-file (f file :direction :input)
    (loop :for l = (read-line f nil)
          :while l
          :count (length l))))

(defun run-slurp-lines ()
  (cond ((probe-file "/usr/share/dict/words")
         (read-many-lines "/usr/share/dict/words"))
        ((probe-file "/usr/dict/words")
         (read-many-lines "/usr/dict/words"))))

(defparameter +alphanumeric+
  "0123456789ABCDEFabcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")

(defvar *table* nil)
(defvar *value* nil)
(defvar *keys* nil)

(defun random-string (length &optional (nchar (length +alphanumeric+)))
  (let ((string (make-string length)))
    (loop for i from 0 below length do
          (setf (aref string i)
                (aref +alphanumeric+ (random nchar))))
    string))

;;; This creates strings of form XXXXabcXXXX that is ith stable prefix and
;;; suffix. This generator is devised to bypass SBCL adaptive string hashing.
(defun random-string-non-adaptive (length nkeys)
  (let* ((string (make-string length))
         (chrlen (length +alphanumeric+))
         (meaningful-length (ceiling (log nkeys chrlen))))
    (loop with b1 = (floor (- length meaningful-length) 2)
          with b2 = (floor (+ length meaningful-length) 2)
          for i from 0 below length
          do (if (<= b1 i b2)
                 (setf (aref string i) (aref +alphanumeric+ (random chrlen)))
                 (setf (aref string i) #\X)))
    string))

;;; This setup generator is primarily meant to test hashing numbers under EQUALP
;;; predicate and stress-test scenarios where floats and rationals overlap or
;;; can overlap under various hasing assumptions. Recommended profiles:
;;;
;;; :AVERAGE simulates expected average workload - fixnums and floats. This
;;; profile also includes with 33% chance nasty fixnums, that is typical 64bit
;;; UUIDs that exceed double float.
;;; 
;;; :DYADIC this is a stress test that excercises paths where floats and
;;; rationals overlap. Note that these benchmarks measure only speed, they don't
;;; validate whether = numbers have the same hash.
;;; 
;;; There are also profiles that excercise individual types:
;;; 
;;; :FIXNUM all keys are fixnums (randomized, no skew towards nasty ones).
;;; :BIGNUM all keys are bignums (50% random, 50% overlap with floats).
;;; :RATIO  all keys are ratios  (fix/big,big/fix,fix/fix,big/big - 25% each)
(defun setup-number (&key (table-size 300)
                       (table-test #'equalp)
                       (n-keys (expt 2 16))
                       (prefill 0)
                       (profile :all)
                     &aux (nasty-fixnum (expt 2 53)))
  (setq *table* (make-hash-table :test table-test :size table-size))
  (setq *value* (random 89))
  (labels ((s (x) (ecase (random 2) (0 x) (1 (- x))))
           ;; Randomized integers
           (gen-fixnum () (s (random most-positive-fixnum)))
           (gen-bignum ()
             (ecase (random 2)
               (0 (gen-bignum-random))
               (1 (gen-bignum-dyadic))))
           (gen-bignum-random ()
             (s (+ 1 most-positive-fixnum (random (expt 2 128)))))
           (gen-bignum-dyadic ()
             (s (ash (1+ (random (expt 2 64))) (+ 62 (random 1000)))))
           ;; Randomized ratios
           (gen-ratio ()
             (ecase (random 4)
               (0 (/ (gen-fixnum) (gen-fixnum)))
               (1 (/ (gen-bignum) (gen-fixnum)))
               (2 (/ (gen-fixnum) (gen-bignum)))
               (3 (/ (gen-bignum) (gen-bignum)))))
           ;; Randomized floats (small fractions and large integers)
           (gen-float-small ()
             (ecase (random 4)
               (0 (s (random 20.0s0)))
               (1 (s (random 20.0f0)))
               (2 (s (random 20.0d0)))
               (3 (s (random 20.0l0)))))
           (gen-float-large ()
             (ecase (random 4)
               (0 (s (random most-positive-short-float)))
               (1 (s (random most-positive-single-float)))
               (2 (s (random most-positive-double-float)))
               (3 (s (random most-positive-long-float)))))
           (gen-float ()
             (ecase (random 2)
               (0 (gen-float-small))
               (1 (gen-float-large))))
           ;; Randomized complex (smoke)
           (gen-complex-float ()
             (complex (gen-float) (gen-float)))
           (gen-complex-ratio ()
             (complex (gen-ratio) (gen-ratio)))
           (gen-complex ()
             (ecase (random 2)
               (0 (gen-complex-float))
               (1 (gen-complex-ratio))))
           ;; Dyadic rationals inside binary64  range (11 bit signed exponent)
           ;; Dyadic rationals beyond binary128 range (15 bit signed exponent)
           (gen-dyadic-bignum-inside ()
             (s (expt 2 (random 1022))))
           (gen-dyadic-bignum-beyond ()
             (s (expt 2 (+ 16500 (random 1024)))))
           (gen-dyadic-inside-ratio ()
             (s (/ (random 16) (gen-dyadic-bignum-inside))))
           (gen-dyadic-beyond-ratio ()
             (s (/ (random 16) (gen-dyadic-bignum-beyond))))
           (gen-dyadic-bignum-ratio ()  ; beyond binary64 inside binary128
             (s (/ (gen-bignum) (expt 2 (+ 62 (random 1000))))))
           ;; Consecutive fixnums exceeding binary64 mantissa (2^53)
           (gen-nasty-fixnum () (incf nasty-fixnum))
           ;; Binary64 that overlap with bignums and ratios.
           (gen-float-bignum () (s (float (gen-dyadic-bignum-inside) 1.0d0)))
           (gen-float-ratio  () (s (float (gen-dyadic-inside-ratio)  1.0d0)))
           ;; Number distribution profiles. We don't stress much for even
           ;; distribution, just "roughly mixed".
           (random-key/rational ()
             (ecase (random 3)
               (0 (gen-fixnum))
               (1 (gen-bignum))
               (2 (gen-ratio))))
           (random-key/float ()
             (gen-float))
           (random-key/complex ()
             (gen-complex))
           (random-key/dyads ()
             (ecase (random 8)
               (0 (gen-dyadic-bignum-inside))
               (1 (gen-dyadic-inside-ratio))
               (2 (gen-dyadic-bignum-beyond))
               (3 (gen-dyadic-beyond-ratio))
               (4 (gen-dyadic-bignum-ratio))
               (5 (gen-nasty-fixnum))
               (6 (gen-float-bignum))
               (7 (gen-float-ratio))))
           ;; Random numbers (no skew towards dyadic rationals).
           (profile-avg ()
             (ecase (random 4)
               (0 (random-key/fixnum))
               (1 (random-key/float))
               (2 (gen-nasty-fixnum))))
           ;; Floats and numbers rigged towards dyadic overlap.
           (profile-dya ()
             (ecase (random 2)
               (0 (random-key/float))
               (1 (random-key/dyads))))
           ;; Fair and rigged numbers together.
           (profile-all ()
             (ecase (random 4)
               (0 (random-key/rational))
               (1 (random-key/float))
               (2 (random-key/dyads))
               (3 (random-key/complex)))))
    (setq *keys*
          (loop repeat n-keys
                for key = (ecase profile
                            (:all (profile-all))
                            (:dyadic (profile-dya))
                            (:average (profile-avg))
                            (:rational (random-key/rational))
                            (:float (random-key/float))
                            (:nasty (gen-nasty-fixnum))
                            (:fixnum (gen-fixnum))
                            (:bignum (gen-bignum))
                            (:ratio  (gen-ratio)))
                collect key
                do (unless (zerop prefill)
                     (decf prefill)
                     (setf (gethash key *table*) *value*))))))

(defun setup-mixbag (&key (table-size 300)
                          (n-keys (expt 2 14))
                          (prefill 0))
  (setq *table* (make-hash-table :test 'equalp :size table-size))
  (setq *value* (random 89))
  (setq *keys*
        (loop repeat n-keys
              for key = (ecase (random 5)
                          (0 (random most-positive-fixnum))
                          (1 (random 33.2))
                          (2 (char "0123456789" (random 10)))
                          (3 (vector 1 "x" #\8 (random 3.4)))
                          (4 (string "jd was here \o/")))
              collect key
              do (unless (zerop prefill)
                   (decf prefill)
                   (setf (gethash key *table*) *value*)))))

(defun setup-string (&key (table-size 300)
                          (n-keys (expt 2 16))
                          (prefill 0)
                          (string-length 32)
                          (charset-length (length +alphanumeric+))
                          (anti-adaptive-p nil))
  (setq *table* (make-hash-table :test 'equal :size table-size))
  (setq *value* (random 42))
  (setq *keys*
        (loop repeat n-keys
              for key = (if anti-adaptive-p
                            (random-string-non-adaptive string-length n-keys)
                            (random-string string-length charset-length))
              collect key
              do (unless (zerop prefill)
                   (decf prefill)
                   (setf (gethash key *table*) *value*)))))

(defun setup-fixnum (&key (table-size 300)
                          (n-keys (expt 2 20))
                          (prefill 0)
                          (max-value most-positive-fixnum))
  (setq *table* (make-hash-table :test 'eql :size table-size))
  (setq *value* (random 89))
  (setq *keys*
        (loop repeat n-keys
              for key = (random max-value)
              collect key
              do (unless (zerop prefill)
                   (decf prefill)
                   (setf (gethash key *table*) *value*)))))

;;; This test excercises insert, map, get and upsert.
(defun bench-htable ()
  (let ((table *table*)
        (value *value*))
    (loop for i from 0
          for key in *keys*
          do (setf (gethash key table) (+ value i)))
    (maphash (lambda (key value)
               (incf (gethash key table) value))
             table)))

;;; This test excercises alternating insert and remove.
(defun bench-setrem ()
  (let ((table *table*)
        (value *value*))
    (loop for i from 0 by 4
          for (k1 k2 k3 k4) on *keys* by #'cddddr
          do (setf (gethash k1 table) (+ value i))
             (setf (gethash k2 table) (+ value i))
             (setf (gethash k3 table) (+ value i))
             (setf (gethash k4 table) (+ value i))
             (remhash k1 table)
             (remhash k2 table)
             (remhash k3 table)
             (remhash k4 table))))

(defun bench-sxhash ()
  (let ((table *table*)
        (value *value*))
    (loop for i from 0
          for key in *keys*
          do (setf value (logxor value (sxhash key))))
    value))

;; EOF
