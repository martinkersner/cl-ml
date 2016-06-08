;;;; Martin Kersner, m.kersner@gmail.com
;;;; 2016/06/07
;;;;
;;;; Logistic regression example for two dimensional data with binary labels.

(load "csv_reader")
(load "logistic_regression")

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
(defparameter *dataset-path* "dataset_small.csv")
(defparameter *dataset* (matrix-from-data (read-csv *dataset-path*)))

;;; Data preprocessing.
(defparameter *label-col-idx* 2)
(defparameter *data* (remove-col *label-col-idx* *dataset*))
(defparameter *labels* (matrix-from-data (nth-col *label-col-idx* *dataset*)))

;;; Logistic regression computation.
(defparameter *weights* (matrix-data (grad-ascent *data* *labels*)))
;(defparameter *weights* (matrix-data (grad-ascent *data* *labels* :lr 0.01)))
;(defparameter *weights* (matrix-data (grad-ascent *data* *labels* :lr 0.001 :max_iter 10)))

;;; Processing weights for plotting decision boundary.
(defparameter *col-idx* 0)
(defparameter *x-min* (nth-col-min *col-idx* *data*))
(defparameter *x-max* (nth-col-max *col-idx* *data*))
(defparameter *y-min* (compute-y-coordinate *weights* *x-min*))
(defparameter *y-max* (compute-y-coordinate *weights* *x-max*))

;;; Plot decision boundary.
(ext:shell
  (concatenate-with-space (list "./plot" *dataset-path*
                                         *x-min* *y-min*
                                         *x-max* *y-max*)))
