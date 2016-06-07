;;;; Martin Kersner, m.kersner@gmail.com
;;;; 2016/06/07
;;;;
;;;; Logistic regression example

(load "csv_reader")
(load "logistic_regression")

; load data
(defparameter *dataset*
  (matrix-from-data (read-csv "dataset_small.csv")))

; training data preparation
(defparameter *label-col* 2)

(defparameter *data* (remove-col *label-col* *dataset*))
(defparameter *labels* (matrix-from-data (nth-col *label-col* *dataset*)))

(grad-ascent *data* *labels*) 
;(grad-ascent *data* *labels* :lr 0.01)
;(grad-ascent *data* *labels* :lr 0.001 :max_iter 10)
