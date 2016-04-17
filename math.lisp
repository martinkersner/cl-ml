(defparameter *sizes* '(784 30 10))

(defparameter *a* (make-array '(4 3) 
  :initial-contents '((0 1 2) (3 4 5) (6 7 8) (9 10 11))))

(defparameter *b* (make-array '(2 4) 
  :initial-contents '((1 2 3 4) (5 6 7 8))))

; krzysz00
; http://stackoverflow.com/questions/12327237/common-lisp-how-to-access-a-row-of-a-certain-multi-dimension-array
(defun array-row-slice (arr row)
  (make-array (array-dimension arr 1) 
    :displaced-to arr 
    :displaced-index-offset (* row (array-dimension arr 1))))


(defun column-major-aref (arr idx)
  (let* ((num_row (array-dimension arr 0))
         (num_col (array-dimension arr 1))
         (col_idx (floor (/ idx num_row)))
         (row_idx (mod idx num_row)))
  (row-major-aref arr (+ col_idx (* row_idx num_col)))))
