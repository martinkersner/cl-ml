;;;; Martin Kersner, m.kersner@gmail.com
;;;; 2016/04/25
;;;;
;;;; Logistic regression

(load "matrix")
(load "csv_reader")

; load data
(defparameter *dataset*
  (matrix-from-data (read-csv "dataset_small.csv")))

; training data preparation
(defparameter *label-col* 2)

(defparameter *data* (remove-col *label-col* *dataset*))
(defparameter *labels* (matrix-from-data (nth-col *label-col* *dataset*)))

(defun grad-ascent (data_mat labels_mat &key (lr 0.001) (max_iter 500))
  (let* ((data_mat (prefix-const-val 1.0 data_mat))
         (n (matrix-cols data_mat))
         (weights (initialize-matrix n 1 1))
         (data_mat_T (transpose data_mat))
         (h)
         (err))

    (dotimes (i max_iter)
      (setf h (sigmoid (dot data_mat weights)))
      (setf err (subtract labels_mat h))
      (setf weights (add weights (dot (multiply lr data_mat_T) err))))

  weights))

(defun stochastic-grad-ascent (data_matrix labels_matrix &key (max_iter 500))
  (let* ((data_mat (append-const-val 1.0 data_matrix))
         (m (matrix-rows data_mat))
         (n (matrix-cols data_mat))
         (weights (initialize-matrix n 1 1))
         (h)
         (err)
         (output))

    (dotimes (i max_iter)
      (let ((data_index (iota m))
            (rand_index)
            (lr)
            (current_data)
            (current_label))

      (dotimes(j m)
        (setf lr (+ 0.01 (/ 4 (+ 1.0 i j)))) ; why 4?
        (setf rand_index (random (length data_index)))
        (setf current_data (nth-row rand_index data_mat))
        (setf current_label (nth-row rand_index label_mat))

        ;(setf h (sigmoid (dot current_data weights)))
        ;(setf err (subtract current_label h))
        ;(setf weights (add weights (dot (multiply lr current_data) err)))

        ;(setf data_index (remove-nth rand_index data_index))
        )

  ;(setf output
  ;weights
  ;current_data
  ;)
     )
      )

  ;output
  ;weights
   ))
