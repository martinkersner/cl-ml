; Martin Kersner, m.kersner@gmail.com
; 2016/04/25

; Logistic regression

(load "math")
(load "dataset1")

(defun sigmoid-matrix (mat)
  (let ((n (array-dimension mat 0))
        (m (array-dimension mat 1)))

  (dotimes (rows n)
    (dotimes (cols n)
      (setf (aref mat rows cols) (sigmoid (aref mat rows cols)))))
 
  mat))

(defun sigmoid (num)
  (/ 1 (+ 1 (exp (- num)))))

(defun grad-ascent (data_matrix labels_matrix &key (lr 1) (max_iter 500))
  (let* ((n (array-dimension data_matrix 1))
         (weights (make-array (list n 1) :initial-element 1))
         (data_matrix_T (transpose data_matrix))
         (h)
         (err))

    (dotimes (i max_iter)
      (setf h (dot data_matrix weights))
      ;(setf h (sigmoid-matrix (dot data_matrix weights)))
      ;(setf err (- labels_matrix - h))
      ;(setf weights (+ weights (* lr data_matrix_T * err)))
    )

  h
))
