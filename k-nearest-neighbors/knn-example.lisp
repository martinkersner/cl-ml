;;;; Martin Kersner, m.kersner@gmail.com
;;;; 2016/08/16
;;;;
;;;; k-nearest neighbors example.

(in-package :lispml)

; load data
(defparameter *dataset*
  (matrix-from-data (read-csv "datasets/logistic-regression/dataset_small.csv")))

; training data preparation
(defparameter *label-col* 2)

(defparameter *data* (remove-col *label-col* *dataset*))
(defparameter *labels* (matrix-from-data (nth-col *label-col* *dataset*)))

;(defparameter *row-data* (matrix-from-data '((2 5))))
(defparameter *row-data* (matrix-from-data '((-1 8))))

(defparameter *k* 4)

(princ (knn *row-data* *data* *labels* *k*))
