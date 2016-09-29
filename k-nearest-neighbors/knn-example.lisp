;;;; Martin Kersner, m.kersner@gmail.com
;;;; 2016/08/16
;;;;
;;;; k-nearest neighbors example.

(in-package :lispml)

;;; Load data
(defparameter *dataset*
  (matrix-from-data (read-csv "datasets/logistic-regression/dataset_small.csv")))

;;; Training data preparation
(defparameter *label-col* 2)

(defparameter *data* (remove-col *label-col* *dataset*))
(defparameter *labels* (matrix-from-data (nth-col *label-col* *dataset*)))

;(defparameter *x-test* '((2 5)))
;(defparameter *x-test* '((-1 8)))
(defparameter *x-test* '((0 8.5)))

(defparameter *row-data* (matrix-from-data *x-test*))

(defparameter *k* 4)

(defparameter *y-test*
  (knn *row-data* *data* *labels* *k*))

(print *y-test*)

;;; Plot data
(ext:shell
  (concatenate-with-space (list "./plot-2d.bash" *dataset-path*
                                                 (caar *x-test*)
                                                 (cadar *x-test*)
                                                 *y-test*)))
