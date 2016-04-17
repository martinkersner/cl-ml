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

;http://aima.cs.berkeley.edu/lisp/utilities/utilities.lisp
(defun iota (n &optional (start-at 0))
  "Return a list of n consecutive integers, by default starting at 0."
  (if (<= n 0) nil (cons start-at (iota (- n 1) (+ start-at 1)))))

(defun apply-column-major-ref (arr l)
  (if (eq l nil) 
    nil 
    (cons (column-major-aref arr (car l)) (apply-column-major-ref arr (cdr l))))) 

(defun array-col-slice (arr col)
  (let* ((num_row (array-dimension arr 0))
         (seq (iota num_row))
         (start_id (* col num_row)))
  (apply-column-major-ref arr (mapcar #'+ seq (make-list (length seq) :initial-element start_id)))))
