; Martin Kersner, m.kesner@gmail.com
; 2016/04/17

; TODO control dimension of matrices before multiplying

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

(defun create-matrix-indices (rows cols orig_cols)
  (if (eq rows 0)
      nil
      (cons (cons (1- rows) (1- cols))
            (if (eq (1- cols) 0)
              (create-matrix-indices (1- rows) orig_cols orig_cols)
              (create-matrix-indices rows (1- cols) orig_cols)))))

(defun dot (mat_left mat_right)
  (let* ((rows (array-dimension mat_left 0))
         (cols (array-dimension mat_right 1))
         (idxs (create-matrix-indices rows cols cols)))
  (dot-rec (make-array (list rows cols)) mat_left mat_right idxs)))

(defun dot-rec (mat_res mat_left mat_right idxs)
  (if (eq (car idxs) nil)
    mat_res
    (let* ((row_idx (caar idxs))
           (col_idx (cdar idxs))
           (row_vec (map 'list #'identity (array-row-slice mat_left row_idx)))
           (col_vec (array-col-slice mat_right col_idx)))

    (dot-rec (mul_vec_of_mat mat_res row_idx col_idx row_vec col_vec) mat_left mat_right (cdr idxs)))))

; Multiply element-wise two vectors and save result to determined location of matrix.
; Return updated matrix.
(defun mul_vec_of_mat (mat row_idx col_idx row_vec col_vec)
  (setf (aref mat row_idx col_idx) (apply #'+ (mapcar #'* row_vec col_vec)))
  mat)

; https://rosettacode.org/wiki/Matrix_transposition#Common_Lisp
; Tranpose matrix.
(defun transpose (A)
  (let* ((m (array-dimension A 0))
         (n (array-dimension A 1))
         (B (make-array `(,n ,m) :initial-element 0)))
    (loop for i from 0 below m do
          (loop for j from 0 below n do
                (setf (aref B j i)
                      (aref A i j))))
    B))

(define-condition matrix-error (error)
  ((text :initarg :text :reader text)))

(defun equal-matrix-dim (A B)
  (let ((n_A (array-dimension A 0))
        (m_A (array-dimension A 1))
        (n_B (array-dimension B 0))
        (m_B (array-dimension B 1)))

    (if (or (not (= n_A n_B)) 
            (not (= m_A m_B)))
      (error 'matrix-error :text "Matrices do not have the same dimensions."))
  ))

(defun equal-matrix-dim2 (n_A m_A n_B m_B)
  (if (or (not (= n_A n_B)) 
      (not (= m_A m_B)))
    (error 'matrix-error :text "Matrices do not have the same dimensions.")))

; Element-wise operation op on matrices A and B.
(defun matrix-elwise-op (A B op)
  (let ((n_A (array-dimension A 0))
        (m_A (array-dimension A 1))
        (n_B (array-dimension B 0))
        (m_B (array-dimension B 1))
        (C))

    ; control matrix dimensions
    (equal-matrix-dim2 n_A m_A n_B m_B)
    (setf C (make-array (list n_A m_A)))

    ; element-wise subtract
    (dotimes (n n_A)
      (dotimes (m m_A)
        (setf (aref C n m) (funcall op (aref A n m) (aref B n m)))))

  C))

; Subtract two matrices element-wise.
; Creates new matrix that stores result values of subtraction.
(defun subtract (A B)
  (matrix-elwise-op A B (lambda (x y) (- x y))))

; Add two matrices element-wise.
; Creates new matrix that stores result values of addition.
(defun add (A B)
  (matrix-elwise-op A B (lambda (x y) (+ x y))))

; Multiply matrix A with constant b.
(defun multiply (b A)
  (let* ((n_A (array-dimension A 0))
        (m_A (array-dimension A 1))
        (C (make-array (list n_A m_A))))

    (dotimes (n n_A)
      (dotimes (m m_A)
        (setf (aref C n m) (* (aref A n m) b))))

  C))

(defun sigmoid-base (num)
  (/ 1 (+ 1 (exp (- num)))))

(defun sigmoid (mat)
  (let ((n (array-dimension mat 0))
        (m (array-dimension mat 1)))

  (dotimes (rows n)
    (dotimes (cols m)
      (setf (aref mat rows cols) (sigmoid-base (aref mat rows cols)))))
 
  mat))
