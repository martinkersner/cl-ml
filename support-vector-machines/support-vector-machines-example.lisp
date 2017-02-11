;;;; Martin Kersner, m.kersner@gmail.com
;;;; 2017/01/24 
;;;;
;;;; Support Vector Machines example

;;; INITIALIZATION
(defparameter *svm* (make-instance 'support-vector-machines))

;;; LOAD DATASET
(multiple-value-setq (train-data train-labels) 
  (load-dataset 
    "datasets/support-vector-machines/dataset.txt"
    2))

;;; TRAINING
(defparameter *params*
  (generate-params '((C       0.6)
                     (toler   0.002)
                     (maxiter 40))))
(fit *svm* train-data train-labels *params*)

(print
  (predict *svm* (matrix-from-data '((3.542485 1.977398)      ; -1
                                     (7.551510 -1.580030))))) ; +1
