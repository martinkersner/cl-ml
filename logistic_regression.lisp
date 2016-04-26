; Martin Kersner, m.kersner@gmail.com
; 2016/04/25

; Logistic regression

(load "math")
(load "utils")
(load "dataset1")

; training data preparation
(defparameter *label-col* 2)

(defparameter *data*
  (remove-col *dataset1* *label-col*))

(defparameter *labels*
  (list-to-col-vector (array-col-slice *dataset1* *label-col*)))

(defun grad-ascent (data_matrix labels_matrix &key (lr 0.001) (max_iter 500))
  (let* ((data_matrix (append-const-val data_matrix 1.0))
         (n (array-dimension data_matrix 1))
         (weights (make-array (list n 1) :initial-element 1))
         (data_matrix_T (transpose data_matrix))
         (h)
         (err))

    (dotimes (i max_iter)
      (setf h (sigmoid (dot data_matrix weights)))
      (setf err (subtract labels_matrix h))
      (setf weights (add weights (dot (multiply lr data_matrix_T) err))))

  weights))
