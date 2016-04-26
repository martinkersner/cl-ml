; Martin Kersner, m.kersner@gmail.com
; 2016/04/25

; Logistic regression

(load "math")
(load "dataset1")

(defun sigmoid (num)
  (/ 1 (+ 1 (exp (- num)))))

(defun sigmoid-matrix (mat)
  (let ((n (array-dimension mat 0))
        (m (array-dimension mat 1)))

  (dotimes (rows n)
    (dotimes (cols m)
      (setf (aref mat rows cols) (sigmoid (aref mat rows cols)))))
 
  mat))

(defun grad-ascent (data_matrix labels_matrix &key (lr 0.001) (max_iter 500))
  (let* ((n (array-dimension data_matrix 1))
         (weights (make-array (list n 1) :initial-element 1))
         (data_matrix_T (transpose data_matrix))
         (h)
         (err))

    (dotimes (i max_iter)
      (setf h (sigmoid-matrix (dot data_matrix weights)))
      (setf err (subtract-matrices labels_matrix h))
      (setf weights (sum-matrices weights (dot (const-mult-matrix lr data_matrix_T) err))))

  weights))
