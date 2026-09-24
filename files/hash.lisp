;; hashtable and READ-LINE benchmarking code
;;
;; some code by Paul Foley
;; Time-stamp: <2016-05-10 13:11:57 jack>

(defpackage :cl-bench.hash
  (:use :common-lisp)
  (:export #:run-slurp-lines
           #:hash-strings
           #:hash-integers
           #:compute-sxhash
           #:compute-sxhash/small
           #:compute-sxhash/large
           #:compute-sxhash/fixnum
           #:compute-sxhash/mixbag))

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

(defparameter +digit+ "0123456789ABCDEF")

(defparameter +digits-needed+
  #((10 100 1000 10000 100000 10000000 100000000 536870911)
    (16 256 4096 65536 1048576 16777216 268435456 4294967296 536870911)))

(defvar *table* nil)

(defun fixnum-to-string (n base)
  (declare (fixnum n base))
  (let* ((tsize (position-if (lambda (x) (> (the fixnum x) n))
                            (aref +digits-needed+ (ash base -4))))
         (result (make-string (1+ tsize))))
    (loop for i fixnum from tsize downto 0 with q fixnum = n and r fixnum = 0
      do (multiple-value-setq (q r) (floor q base))
         (setf (schar result i) (aref +digit+ r)))
    result))

;; CMUCL-18c seems to run into a bug here: it mistakenly declares
;; counter to be a fixnum
(defun hash-strings (&optional (size 300))
  (declare (fixnum size))
   (setq *table* (make-hash-table :test #'equal :size size))
   (dotimes (i 100000)
     (setf (gethash (fixnum-to-string i 16) *table*) i))
   (maphash (lambda (key value) (incf (gethash key *table*) value)) *table*))
  
(defun hash-integers (&optional (size 300))
  (declare (fixnum size))
  (setq *table* (make-hash-table :test #'eql :size size))
  (dotimes (i 100000)
    (setf (gethash i *table*) (1+ i)))
  (maphash (lambda (key value) (incf (gethash key *table*) value)) *table*))

(defun compute-sxhash (&optional (size 32))
  (declare (fixnum size))
  (let ((string (make-string size :initial-element
                             (ecase (random 3) (0 #\x) (1 #\y) (2 #\z))))
        (result 0))
    (dotimes (i 4000000 result)
      (setf (char string (random size))
            (ecase (random 3) (0 #\x) (1 #\y) (2 #\z)))
      (setf result (logxor result (sxhash string))))))

(defun compute-sxhash/small ()
  (compute-sxhash 6))

(defun compute-sxhash/large ()
  (compute-sxhash 1024))

(defun compute-sxhash/fixnum (&optional (max most-positive-fixnum))
  (let ((result 0)
        value1 value2)
    (dotimes (i 2000 result)
      (setf value1 (random max))
      (setf value2 (- (random max)))
      (dotimes (i 2000)
        (setf result (logxor result (sxhash value1)))
        (setf result (logxor result (sxhash value2)))))))

(defun compute-sxhash/mixbag (&optional (size 16))
  (let ((value (list* 0
                      (loop repeat size
                            collect (ecase (random 5)
                                      (0 (random most-positive-fixnum))
                                      (1 (random 33.2))
                                      (2 (char "0123456789" (random 10)))
                                      (3 (vector 1 "x" #\8 (random 3.4)))
                                      (4 (string "jd was here \o/")))))))
    (dotimes (i 4000000 value)
      (setf (car value) (sxhash value)))))

;; EOF
