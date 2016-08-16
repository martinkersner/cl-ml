;;;; Martin Kersner, m.kersner@gmail.com
;;;; 2016/06/12 

(in-package :lispml)

;;; Load data.
(defparameter *dataset-path* "datasets/logistic-regression/dataset_small.csv")
(defparameter *dataset* (matrix-from-data (read-csv *dataset-path*)))

;;; Data preprocessing.
(defparameter *label-col-idx* 2)
(defparameter *data* (remove-col *label-col-idx* *dataset*))
(defparameter *labels* (matrix-from-data (nth-col *label-col-idx* *dataset*)))

;;; Define neural network.
(defparameter *epoch-num* 4)
(defparameter *mini-batch-size* 50)
(defparameter *lr* 0.0001)
(defparameter *nn* (make-instance 'neural-network :nn-dims '(2 4 2)))

(SGD *nn* *data* *labels* *epoch-num* *mini-batch-size* *lr*
     *data* *labels*)

;(defparameter *x* (matrix-from-data '((8)(7))))
;(defparameter *y* (matrix-from-data '((1))))
;(defparameter *x-row* (matrix-from-data '((8 7)(7 6))))
;(defparameter *y-row* (matrix-from-data '((1 0))))

;;(setf b (feed-forward *nn* *x*))
;(multiple-value-setq (grad-b grad-w) (backpropagation *nn* *x* *y*))

;(princ (evaluate *nn* *x-row* *y-row*))
