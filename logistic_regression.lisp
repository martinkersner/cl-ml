; Martin Kersner, m.kersner@gmail.com
; 2016/04/25

; Logistic regression

(load "math")
(load "dataset1")

(defun grad-ascent (data_matrix labels_matrix &key (lr 0.001) (max_iter 500))
  (let* ((n (array-dimension data_matrix 1))
         (weights (make-array (list n 1) :initial-element 1))
         (data_matrix_T (transpose data_matrix))
         (h)
         (err))

    (dotimes (i max_iter)
      (setf h (sigmoid (dot data_matrix weights)))
      (setf err (subtract labels_matrix h))
      (setf weights (sum weights (dot (multiply lr data_matrix_T) err))))

  weights))
