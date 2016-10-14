;;;; Martin Kersner, m.kersner@gmail.com
;;;; 2016/08/12
;;;;
;;;; 1D linear regression example.

(in-package :lispml)

;;; INITIALIZATION
(defparameter *linreg* (make-instance 'linear-regression))

;;; TRAINING
(defparameter *train-dataset-path* "datasets/linear-regression/pizza-train.csv")
(multiple-value-setq (train-data train-labels) (load-dataset *train-dataset-path* 1))

;;; Train model.
(fit *linreg* train-data train-labels)

;;; TESTING
(defparameter *test-dataset-path* "datasets/linear-regression/pizza-test.csv")
(multiple-value-setq (test-data test-labels) (load-dataset *test-dataset-path* 1))

;;; Evaluate model.
(defparameter *r-squared* (score *linreg* test-data test-labels))
(print *r-squared*)
