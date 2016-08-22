;;;; Martin Kersner, m.kersner@gmail.com
;;;; 2016/08/12
;;;;
;;;; 1D linear regression example.

(in-package :lispml)

(defun load-dataset (dataset-path)
  (let* (;;; Load data.
        (dataset (matrix-from-data (read-csv dataset-path)))

        ;;; Data preprocessing.
        (label-col-idx (- (matrix-cols dataset) 1)) ; labels are located in the last column of data matrix
        (data   (nth 0 (transpose-list (nth-col 0 dataset))))
        (labels (nth 0 (transpose-list (nth-col label-col-idx dataset)))))

  (values data labels)))

;;; TRAINING
(defparameter *train-dataset-path* "datasets/linear-regression/pizza-train.csv")
(multiple-value-setq (train-data train-labels) (load-dataset *train-dataset-path*))

;;; Linear regression computation.
(defparameter *weights* (train-linear-regression-1D train-data train-labels))

;;; TESTING
(defparameter *test-dataset-path* "datasets/linear-regression/pizza-test.csv")
(multiple-value-setq (test-data test-labels) (load-dataset *test-dataset-path*))

;;; Evaluate model.
(defparameter *r-squared* (test-linear-regression-1D (nth 0 *weights*) (nth 1 *weights*) test-data test-labels))

(print *r-squared*)
