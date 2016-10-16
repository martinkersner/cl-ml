;;;; Martin Kersner, m.kersner@gmail.com
;;;; 2016/08/16
;;;;
;;;; k-nearest neighbors example.

(in-package :lispml)

;;; Load data
(defparameter *dataset-path* "datasets/logistic-regression/dataset_small.csv")
(defparameter *dataset*
  (matrix-from-data (read-csv *dataset-path*)))

;;; Training data preparation
(defparameter *label-col* 2)

(defparameter *train-data* (remove-col *label-col* *dataset*))
(defparameter *train-labels* (matrix-from-data (nth-col *label-col* *dataset*)))

;(defparameter *x-test* '((2 5))) ; 1 class
;(defparameter *x-test* '((-1 8))) ; 0 class
(defparameter *x-test* '((0 8.5))) ; 0 class

(defparameter *row-data* (matrix-from-data *x-test*))

;;; INITIALIZATION
(defparameter *knn* (make-instance 'k-nearest-neighbors))

;;; Train model.
(fit *knn* *train-data* *train-labels*)

(defparameter *params*
  (generate-params '((k 3))))

(defparameter *y-test*
  (predict *knn* *row-data* *params*))

(print *y-test*)

;;; Plot data
(ext:shell
  (concatenate-with-space (list "./plot-2d.bash" *dataset-path*
                                                 (caar *x-test*)
                                                 (cadar *x-test*)
                                                 *y-test*)))
