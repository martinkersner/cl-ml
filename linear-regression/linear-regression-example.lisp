;;;; Martin Kersner, m.kersner@gmail.com
;;;; 2016/08/22
;;;;
;;;; Linear regression example.

(in-package :cl-ml)

;;; INITIALIZATION
(defparameter *linreg* (make-instance 'linear-regression))

;;; TRAINING
(defparameter *train-dataset-path* "datasets/linear-regression/pizza-nd-train.csv")
(multiple-value-setq (train-data train-labels) (load-dataset *train-dataset-path* 2))

;;; Train model.
(defparameter *params*
  (generate-params '((num-epoch 10)
                     (lr        0.0005))))
(fit *linreg* train-data train-labels *params*)

;;; PREDICTION
(print
  (predict *linreg* (matrix-from-data '((8 2)(10 10)))))

;;; TESTING
(defparameter *test-dataset-path* "datasets/linear-regression/pizza-nd-test.csv")
(multiple-value-setq (test-data test-labels) (load-dataset *test-dataset-path* 2))

;;; Evaluate model.
(defparameter *r-squared* (score *linreg* test-data test-labels))
(print *r-squared*)
