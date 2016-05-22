;;; html-report.lisp
;;; 
;;; Time-stamp: <2016-05-20 14:36:27 jack>
;;; 
;;; Inspired by a benchmark page example from lispm.de kindly shared
;;; by Rainer Joswig
;;;
;;; * Copyright (c) 2016 Rainer Joswig <joswig@lisp.de>
;;; * Copyright (c) 2016 Daniel Kochmański <daniel@turtleware.eu>
;;; 

(in-package #:cl-bench)

(defun read-benchmarks ()
  (let (data)
    (dolist (f (directory (merge-pathnames "CL-benchmark*.*" *output-dir*)) data)
      (with-open-file (f f :direction :input)
        (let ((*read-eval* nil))
          (push (read f) data))))))

(defun bench-analysis-page ()
  (let* ((data (read-benchmarks))
         (implementations (mapcar #'car data))
         (benchmarks (reverse (mapcar #'first (cdr (first data))))))
    (with-open-file (s #P"/tmp/bench.html" :direction :output :if-exists :supersede)
      (cl-who:with-html-output (s nil :prologue t)
        (cl-who:htm
         (:p "hello world!")
         (:table
          :border 1
          (:tr (:td :style "max-width: 100px;")
               (dolist (i implementations)
                 (cl-who:htm (:th (cl-who:str i)))))
          (dolist (b benchmarks)
            (let* ((results (loop :for i in implementations
                               :collect (let* ((id (cdr (assoc i data :test #'string=)))
                                               (ir (third (assoc b id :test #'string=))))
                                          (if (numberp ir)
                                              ir
                                              -1))))
                   (best (apply #'min results)))
              (cl-who:htm
              (:tr (:th (cl-who:str b))
                   (dolist (r results)
                     (flet ((cell-color (value best)
                              (cond ((= value -1) "#fff")
                                    ((> value (* 4 best)) "#b4b")
                                    ((> value (* 2 best)) :red)
                                    ((> value (* 1.6 best)) :orange)
                                    ((> value (* 1.25 best)) "#4b4")
                                    ((<= value (* 1.25 best)) :green)))
                            (cell-style (color)
                              (format nil "background-color: ~A;" color)))
                       (cl-who:htm (:td :style (cell-style (cell-color r best))
                                        (when r (cl-who:str r))))
                       )))
              )))))))
    benchmarks))
