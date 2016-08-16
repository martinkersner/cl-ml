;;;; Martin Kersner, m.kersner@gmail.com
;;;; 2016/06/07
;;;;
;;;; Logistic regression example for two dimensional data with binary labels.

(in-package :lispml)

(defun compute-y-coordinate (weights x)
  (let ((w0 (caar weights))
        (w1 (caadr weights))
        (w2 (caaddr weights)))

    (/ (- (- w0) (* w1 x)) w2)))

(defun concatenate-with-space (str-lst &optional (complete ""))
  (let* ((space " ")
         (item-tmp (car str-lst))
         (item (if (stringp item-tmp)
                 item-tmp
                 (write-to-string item-tmp))))

    (if str-lst
      (concatenate-with-space
        (cdr str-lst)
        (concatenate 'string complete item space))
      complete)))

;;; Load data.
(defparameter *dataset-path* "datasets/logistic-regression/dataset_small.csv")
(defparameter *dataset* (matrix-from-data (read-csv *dataset-path*)))

;;; Data preprocessing.
(defparameter *label-col-idx* 2)
(defparameter *data* (remove-col *label-col-idx* *dataset*))
(defparameter *labels* (matrix-from-data (nth-col *label-col-idx* *dataset*)))

(defparameter *mean* (mean-cols *data*))
(defparameter *std* (std-cols *data* *mean*))
;(setf *data* (normalize *data* *mean* *std*))

;;; Logistic regression computation.
(defparameter *weights* (matrix-data (grad-ascent *data* *labels*)))
;(defparameter *weights* (matrix-data (grad-ascent *data* *labels* :lr 0.01)))
;(defparameter *weights* (matrix-data (grad-ascent *data* *labels* :lr 0.001 :max_iter 10)))

;;; Stochastic logistic regression.
;(defparameter *weights* (matrix-data (stochastic-grad-ascent *data* *labels*)))

;;; Processing weights for plotting decision boundary.
(defparameter *col-idx* 0)
(defparameter *x-min* (nth-col-min *col-idx* *data*))
(defparameter *x-max* (nth-col-max *col-idx* *data*))
(defparameter *y-min* (compute-y-coordinate *weights* *x-min*))
(defparameter *y-max* (compute-y-coordinate *weights* *x-max*))

;;; Denormalize points of decision boundary.
;(defparameter *mean-list* (matrix-data-peel *mean*))
;(defparameter *std-list* (matrix-data-peel *std*))
;(defparameter *mean-x* (car *mean-list*))
;(defparameter *mean-y* (cadr *mean-list*))
;(defparameter *std-x* (car *std-list*))
;(defparameter *std-y* (cadr *std-list*))
;(setf *x-min* (+ (* *x-min* *std-x*) *mean-x*))
;(setf *x-max* (+ (* *x-max* *std-x*) *mean-x*))
;(setf *y-min* (+ (* *y-min* *std-y*) *mean-y*))
;(setf *y-max* (+ (* *y-max* *std-y*) *mean-y*))

;;; Plot decision boundary.
(ext:shell
  (concatenate-with-space (list "./plot" *dataset-path*
                                         *x-min* *y-min*
                                         *x-max* *y-max*)))
