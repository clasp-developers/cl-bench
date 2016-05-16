;;; pdf-report.lisp
;;
;; Author: Eric Marsden <emarsden@laas.fr>
;; Time-stamp: <2016-05-16 11:39:25 jack>
;;
;;
;; When loaded into CMUCL, this should generate a report comparing the
;; performance of the different CL implementations which have been
;; tested. Reads the output/CL-benchmark* files to obtain data from
;; previous runs. Requires the cl-pdf library. 

(in-package :cl-user)

(eval-when (:load-toplevel :execute)
  (require :asdf)
  (asdf:oos 'asdf:load-op :uffi)
  (ext:load-foreign "/usr/lib/libz.so")
  (asdf:oos 'asdf:load-op :cl-pdf)
  (asdf:oos 'asdf:load-op :cl-typesetting))

(load #p"defpackage.lisp")
(load #p"support.lisp")
(load #p"tests.lisp")


(in-package :cl-bench)


(defun histogram-maker (title label-names results)
  (lambda (box x y)
    (pdf:in-text-mode
     (pdf:move-text x y)
     (pdf:set-font (pdf:get-font "Helvetica") 11)
     (pdf:show-text title))
    (pdf:draw-object
     (make-instance 'pdf:histogram
                    :x x :y (- y 95)
                    :width 90 :height 90
                    :label-names label-names
                    :labels&colors '(("ignore" (0.0 0.0 1.0)))
                    :series (list results)
                    :y-axis-options '(:min-value -0.5 :title "seconds")
                    :background-color '(0.9 0.9 0.9)))))



;; FIXME annotate each benchmark with estimated allocation volume & peak storage requirement
(defun bench-analysis
    (&optional (filename (merge-pathnames "cl-bench.pdf" *output-dir*)))
  (let (content data groups implementations benchmarks impl-scores impl-labels)
    (dolist (f (directory (merge-pathnames "CL-benchmark*.*" *output-dir*)))
      (ignore-errors
        (with-open-file (f f :direction :input)
          (let ((*read-eval* nil))
            (push (read f) data)))))
    (setf data (sort data #'string< :key #'car))
    (setf implementations (mapcar #'car data))
    (loop :for b :in *benchmarks*
       :do (pushnew (benchmark-group b) groups))
    (setf impl-scores (make-list (length implementations)
                                 :initial-element 0))
    (setf impl-labels (loop :for i :from 0 :below (length implementations)
                         :collect (string (code-char (+ i (char-code #\A))))))
    (setf benchmarks (reverse (mapcar #'first (cdr (first data)))))

    (let ((*break-on-signals* 'condition)
          (header (typeset::compile-text ()
                                         (typeset::paragraph (:h-align :centered
                                                                       :font "Times-Italic" :font-size 10)
                                                             "cl-bench performance benchmarks")
                                         (typeset:hrule :dy 1/2)))
          (footer (lambda (pdf:*page*)
                    (typeset::compile-text (:font "Helvetica" :font-size 9)
                                           (typeset:hrule :dy 1/2)
                                           (typeset::hbox (:align :center :adjustable-p t)
                                                          (typeset::put-string "2004-03-09")
                                                          :hfill
                                                          (typeset::put-string
                                                           (format nil "page ~d"
                                                                   (1+ (position pdf:*page* (typeset::pages pdf:*document*))))))))))
      (typeset::with-document (:author "Éric Marsden"
                                       :title "cl-bench performance results")
        (dolist (group groups)
          (setq content
                (typeset::compile-text (:first-line-indent 0)
                                       (typeset:paragraph (:font "Times-Italic" :font-size 16)
                                                          (typeset:put-string (concatenate 'string (string group) " group")))
                                       (typeset::vspace 10)
                                       (typeset:hrule :dy 1)
                                       (typeset::vspace 40)
                                       (typeset::paragraph (:first-line-indent 0)
                                                           (dolist (bm (remove-if-not (lambda (b) (eql (benchmark-group b) group)) *benchmarks*))
                                                             (let* ((bn (benchmark-name bm))
                                                                    (results (loop :for i :in implementations
                                                                                :collect (let* ((id (cdr (assoc i data :test #'string=)))
                                                                                                (ir (third (assoc bn id :test #'string=))))
                                                                                           (if (numberp ir) (float ir) -0.02)))))
                                                               ;; (typeset::hspace 10)
                                                               (typeset::user-drawn-box
                                                                :inline t :dx 130 :dy 150
                                                                :stroke-fn (histogram-maker bn impl-labels results))
                                                               (typeset::hspace 20)))
                                                           :eop)))
          (typeset::draw-pages content :margins '(72 72 72 72) :header nil :footer footer))
        (setq content
              ;; index of implementation names
              (typeset::compile-text (:first-line-indent 0)
                                     (typeset::paragraph (:font-size 16) "Implementations")
                                     (typeset::vspace 10)
                                     (typeset::hrule :dy 1)
                                     (typeset::vspace 10)
                                     (dotimes (i (length implementations))
                                       (typeset::paragraph (:font "Times-Roman" :font-size 12)
                                                           (typeset::put-string (format nil "~A: ~A~%~%" (nth i impl-labels) (nth i implementations))))
                                       :eol)
                                     :eop :eop))
        (typeset::draw-pages content :margins '(72 72 72 72) :header nil :footer footer)
        (pdf:write-document filename)))))

(bench-analysis)
(quit)

;; EOF
