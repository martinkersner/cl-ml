;;;; Martin Kersner, m.kersner@gmail.com
;;;; 2016/06/12 

;;; Load data
(defparameter *dataset-path* "datasets/logistic-regression/dataset_small.csv")
(defparameter *dataset* (matrix-from-data (read-csv *dataset-path*)))

;;; Data preprocessing
(defparameter *label-col-idx* 2)
(multiple-value-setq (train-data train-labels)
  (load-dataset *dataset-path* *label-col-idx*))

;;; Initialize neural network
(defparameter *nn*
  (make-instance 'neural-network :nn-dims '(2 1)))

;;;; Train model
(defparameter *params*
  (generate-params '((num-epoch       50)
                     (lr              0.3)
                     (mini-batch-size 20))))

(fit *nn* train-data train-labels *params*)

(parameter-summary *nn*)

(defparameter *X* (matrix-from-data '((0 14)
                                      (0.3 0.2))))
(defparameter *y-true* '(0 1))

(format t "~%y-pred: ~d~%y-true: ~d"
        (predict *nn* *X*)
        *y-true*)
