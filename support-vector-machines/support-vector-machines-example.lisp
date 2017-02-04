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

(print (get-b *svm*))
(print (get-alphas *svm*))
