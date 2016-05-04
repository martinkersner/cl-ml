;;;; Martin Kersner, m.kersner@gmail.com
;;;; 2016/04/25
;;;;
;;;; Logistic regression

(load "math2")
(load "dataset2")

; training data preparation
(defparameter *label-col* 2)

(defparameter *data* (remove-col *label-col* *dataset2*))
(defparameter *labels* (matrix-from-data (nth-col *label-col* *dataset2*)))

(defun grad-ascent (data_mat labels_mat &key (lr 0.001) (max_iter 500))
  (let* (
         (data_mat (prefix-const-val 1.0 data_mat))
         (n (matrix-cols data_mat))
         (weights (initialize-matrix n 1 1))
         (data_mat_T (transpose data_mat))
         (h)
         (err)
         )

    (dotimes (i max_iter)
      (setf h (sigmoid (dot data_mat weights)))
      (setf err (subtract labels_mat h))
      (setf weights (add weights (dot (multiply lr data_mat_T) err))))

  weights))

;(defun stochastic-grad-ascent (data_matrix labels_matrix &key (max_iter 500))
;  (let* ((data_matrix (append-const-val data_matrix 1.0))
;         (m (array-dimension data_matrix 0))
;         (n (array-dimension data_matrix 1))
;         (weights (make-array (list n 1) :initial-element 1))
;         (h)
;         (err)
;         (output))
;
;    (dotimes (i max_iter)
;      (let ((data_index (iota m))
;            (rand_index)
;            (lr)
;            (current_data)
;            (current_label))
;
;      (dotimes(j m)
;        (setf lr (+ 0.01 (/ 4 (+ 1.0 i j)))) ; why 4?
;        (setf rand_index (random (length data_index)))
;        (setf current_data (array-row-slice data_matrix rand_index))
;        (setf current_label (make-array '(1 1) :initial-element (aref labels_matrix rand_index 0)))
;
;        ;(setf h (sigmoid (dot current_data weights)))
;        ;(setf err (subtract current_label h))
;        ;(setf weights (add weights (dot (multiply lr current_data) err)))
;
;        (setf data_index (remove-nth rand_index data_index))
;        )
;
;  ;(setf output
;  ;weights
;  ;current_data
;  ;)
;     )
;      )
;
;  ;output
;  weights
;   ))
