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
  (make-instance 'neural-network :nn-dims '(2 3 4 1)))

;;;; Train model.
(defparameter *params*
  (generate-params '((num-epoch       30)
                     (lr              0.5)
                     (mini-batch-size 50))))
(fit *nn* train-data train-labels *params*)

;(print 'biases)
;(print (biases  *nn*))
;(print 'weights)
;(print (weights *nn*))

;(defparameter *x* (matrix-from-data '((8)(7))))
;(defparameter *y* (matrix-from-data '((1))))
;(defparameter *x-row* (matrix-from-data '((8 7)(7 6))))
;(defparameter *y-row* (matrix-from-data '((1 0))))

;(predict *nn* *x*)
;(setf b (feed-forward *nn* *x*))
;(multiple-value-setq (grad-b grad-w) (backpropagation *nn* *x* *y*))

;(princ (evaluate *nn* *x-row* *y-row*))
