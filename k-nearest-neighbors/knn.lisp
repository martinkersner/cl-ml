;;;; Martin Kersner, m.kersner@gmail.com
;;;; 2016/05/08 
;;;;
;;;; k-Nearest Neighbors 

;;; TODO
;;; prepare dataset specially for knn
;;; and also include tests for algorithm verification

(in-package :lispml)

(defun knn (data_row data_mat labels_mat k)
  (let* ((class_count (make-hash-table))
         (labels (matrix-data labels_mat))
         (k_neighbors (subseq (arg-sort-col-mat ; take only the closest k data points
                        (power 0.5 (sum-rows (power 2 (subtract-row data_mat data_row))))) ; Euclidean distance
                       0 k))
         (k_labels (mapcar #'(lambda (x) (car (nth x labels))) k_neighbors))
         (final_class 0)
         (max_count 0))

    ;; count number of class occurences
    (mapcar #'(lambda (x)
                (let ((val (gethash x class_count)))
                   (if val
                     (setf (gethash x class_count) (+ 1 val))
                     (setf (gethash x class_count) 1))
                   ))
            k_labels)

    ;; find class with the highest occurence
    (maphash #'(lambda (k v)
                  (if (> v max_count)
                      (progn
                        (setf max_count v)
                        (setf final_class k))))
             class_count)

  final_class))