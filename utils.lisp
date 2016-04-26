; Martin Kersner, m.kersner@gmail.com
; 2016/04/26

(load "math")

; Append constant number at the beginning of each row.
; Create new matrix with appended values. 
(defun append-const-val (A val)
  (let* ((n (array-dimension A 0))
         (m (1+ (array-dimension A 1)))
         (B (make-array (list n m)))
         (tmp_vals))

  (flet ((update-row (row_idx vals)
            (dotimes (col_idx m)
             (setf (aref B row_idx col_idx) (nth col_idx vals)))))

  (dotimes (i n)
    (setf tmp_vals (append (list val) (map 'list #'identity (array-row-slice A i))))
    (update-row i tmp_vals))

  B)))
