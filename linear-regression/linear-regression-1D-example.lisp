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
(defparameter *params*
  (generate-params '((num-epoch 1)
                     (lr        0.0005))))

(fit *linreg* train-data train-labels *params*)

;;; TESTING
(defparameter *test-dataset-path* "datasets/linear-regression/pizza-test.csv")
(multiple-value-setq (test-data test-labels) (load-dataset *test-dataset-path* 1))

;;; Evaluate model.
(defparameter *r-squared* (score *linreg* test-data test-labels))
(print *r-squared*)

(setf *weights*
      (matrix-data (get-weights *linreg*)))

;;; Processing weights for plotting decision boundary.
(defparameter *col-idx* 0)
(defparameter *x-min* (nth-col-min *col-idx* test-data))
(defparameter *x-max* (nth-col-max *col-idx* test-data))
(defparameter *y-min* ([][] 0 0 (predict *linreg* (matrix-from-data (list (list *x-min*))))))
(defparameter *y-max* ([][] 0 0 (predict *linreg* (matrix-from-data (list (list *x-max*))))))

;;; Plot regression line.
(ext:shell
  (concatenate-with-space (list "./plot-lin-reg-2d.bash" *test-dataset-path*
                                         *x-min* *y-min*
                                         *x-max* *y-max*)))
