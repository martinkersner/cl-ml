;;;; Martin Kersner, m.kersner@gmail.com
;;;; 2016/08/24
;;;;
;;;; Example code for Decision Tree classifier using Iterative Dichotomiser 3 (id3).

(in-package :lispml)

(defun load-dataset (dataset-path)
  (let* (;;; Load data.
        (dataset (matrix-from-data (read-csv dataset-path t)))

        ;;; Data preprocessing.
        (label-col-idx (- (matrix-cols dataset) 1)) ; labels are located in the last column of data matrix
        (data  
          (remove-col label-col-idx dataset))
        (labels 
          (matrix-from-data (nth-col label-col-idx dataset))))

  (values data labels)))

;;; INITIALIZATION
(defparameter *dt* (make-instance 'id3-dt))

;;; TRAINING
(defparameter *train-dataset-path* "datasets/decision-trees/cat-dog.csv")
(multiple-value-setq (train-data train-labels) (load-dataset *train-dataset-path*))

;(print train-data)
;(print train-labels)

;;; Train model.
(fit *dt* train-data train-labels)

;;;; PREDICTION
;(print
;  (predict *dt* (matrix-from-data '((8 2)(10 10)))))
;
;;;; TESTING
;(defparameter *test-dataset-path* "datasets/linear-regression/pizza-nd-test.csv")
;(multiple-value-setq (test-data test-labels) (load-dataset *test-dataset-path*))
;
;;;; Evaluate model.
;(defparameter *r-squared* (score *dt* test-data test-labels))
;(print *r-squared*)
