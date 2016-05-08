;;;; Martin Kersner, m.kersner@gmail.com
;;;; 2016/05/08 
;;;;
;;;; k-Nearest Neighbors 

(load "math2")

(defun knn (data_row, data_mat, labels_mat k)
  (power 2 (subtract-row data_mat data_row))
  )
