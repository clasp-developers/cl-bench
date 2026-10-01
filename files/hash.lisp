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

(defun setup-mixbag (&key (table-size 300)
                          (n-keys (expt 2 14)))
  (setq *table* (make-hash-table :test 'equalp :size table-size))
  (setq *value* (random 89))
  (setq *keys*
        (loop repeat n-keys
              collect (ecase (random 5)
                        (0 (random most-positive-fixnum))
                        (1 (random 33.2))
                        (2 (char "0123456789" (random 10)))
                        (3 (vector 1 "x" #\8 (random 3.4)))
                        (4 (string "jd was here \o/"))))))

(defun setup-string (&key (table-size 300)
                          (n-keys (expt 2 16))
                          (string-length 32)
                          (charset-length (length +alphanumeric+)))
  (setq *table* (make-hash-table :test 'equal :size table-size))
  (setq *value* (random 42))
  (setq *keys*
        (loop repeat n-keys
              collect (random-string string-length charset-length))))

(defun setup-fixnum (&key (table-size 300)
                       (n-keys (expt 2 20))
                       (max-value most-positive-fixnum))
  (setq *table* (make-hash-table :test 'eql :size table-size))
  (setq *value* (random 89))
  (setq *keys*
        (loop repeat n-keys
              collect (random max-value))))

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
