;;;; Martin Kersner, m.kersner@gmail.com
;;;; 2016/08/12
;;;;
;;;; 1D linear regression example.

(in-package :lispml)

;;; TRAINING
;;;
;;; Load data.
(defparameter *dataset-path* "datasets/linear-regression/pizza-train.csv")
(defparameter *dataset* (matrix-from-data (read-csv *dataset-path*)))

;;; Data preprocessing.
(defparameter *label-col-idx* (- (matrix-cols *dataset*) 1)) ; labels are located in the last column of data matrix
(defparameter *data*   (nth 0 (transpose-list (nth-col 0 *dataset*))))
(defparameter *labels* (nth 0 (transpose-list (nth-col *label-col-idx* *dataset*))))

;;; Linear regression computation.
(defparameter *weights* (train-linear-regression-1D *data* *labels*))
