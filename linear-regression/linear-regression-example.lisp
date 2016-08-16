;;;; Martin Kersner, m.kersner@gmail.com
;;;; 2016/08/12
;;;;
;;;; Linear regression example.

(in-package :lispml)

;;; Load data.
(defparameter *dataset-path* "datasets/linear-regression/dataset0.csv")
(defparameter *dataset* (matrix-from-data (read-csv *dataset-path*)))

;;; Data preprocessing.
(defparameter *label-col-idx* 1)
(defparameter *data* (remove-col *label-col-idx* *dataset*))
(defparameter *labels* (matrix-from-data (nth-col *label-col-idx* *dataset*)))

;;; Logistic regression computation.
;(defparameter *weights* (matrix-data (linear-regression *data* *labels*)))
