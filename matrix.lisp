;;;; Martin Kersner, m.kersner@gmail.com
;;;; 2016/04/27 
;;;;
;;;; Math 2.0
;;;; Perform mathematical operations, mainly with matrices (vector is considered
;;;; as matrix as well). Matrices are represented by lists. Most operations have
;;;; strict prototype in order to avoid running additional functions and don't
;;;; control underflow or overflow of dimension indices. Rows and cols are
;;;; counted from zero.

;;; MATRIX CREATION
;;; * (empty-matrix rows cols)
;;; * (empty-matrix-like mat)
;;; * (initialize-matrix rows cols val)
;;; * (rand-norm-matrix rows cols)
;;; * (matrix-from-data data)
;;; * (matrix-from-data-peel data)
;;; * (matrix-data-peel data)
;;;
;;; SINGLE MATRIX OPERATIONS
;;; * (transpose mat)
;;; * (nth-row row mat)
;;; * (nth-col col mat)
;;; * ([] from to mat)
;;; * (remove-col col mat)
;;; * (remove-row col mat)
;;; * (remove-row-list-matrix row list-mat)
;;; * (prefix-const-val val mat)
;;; * TODO (suffix-const-val val mat)
;;; * TODO insert constant value (whole column) at given position
;;; * (matrix-indices rows cols)
;;; * (sigmoid mat)
;;; * (sigmoid-prime mat)
;;; * (shuffle-rows mat)
;;;
;;; MATRIX MULTIPLICATION
;;; ** number of cols of mat1 has to be equal number of rows of mat2
;;; * (dot mat1 mat2)
;;;
;;; ELEMENT-WISE OPERATIONS
;;; ** requires the same dimensions of both matrices
;;; ** uniqueness of parameter order depends on commutative property of employed
;;;    mathematical function
;;; * (add mat1 mat2)
;;; * (subtract mat1 mat2)
;;; * (matrix-mult mat1 mat2)
;;; * (matrix-div mat1 mat2)
;;;
;;; MATRIX & VALUE OPERATIONS
;;; TODO unite names
;;; * (value-matrix-subtract val mat)
;;; * (add-value val mat)
;;; * (multiply val mat)
;;; * (power val mat)
;;;
;;; MATRIX-ROW/COL OPERATIONS
;;; * (subtract-row mat row)
;;; * TODO (subtract-col col mat)
;;; * (subtract-val-col val col mat)
;;; * (sum-rows mat)
;;; * (sum-cols mat)
;;; * (mean-cols mat)
;;; * TODO (mean-rows mat)
;;; *(arg-sort-col-mat col_mat) TODO accept matrices with more than one column
;;; * TODO (arg-sort-row-mat row_mat)
;;; * (nth-col-max col mat)
;;; * (nth-col-min col mat)
;;; * TODO (nth-row-max col mat)
;;; * TODO (nth-row-min col mat)

;;; TODO 
;;; smart operations (add vector to all rows or columns of matrix)
;;; nth-row nth-col base functions for returning complete matrices
;;; rewrite remove-nth function and move to file with list operations
;;; unit test for EVERY FUNCTION
;;; merge similar code from multiply, power, value-matrix-subtract functions together
;;; more inspiration from numpy
;;; generate all functions, keep them in categories

(load "random") ; TODO necessary?
(load "list")

(defstruct matrix rows cols data)

(define-condition matrix-error (error)
  ((text :initarg :text :reader text)))

;;; Compare rows and columns of two matrices and their data.
(defun compare-matrix (mat_a mat_b)
  (cond 
    ((not (equal (matrix-rows mat_a) (matrix-rows mat_b))) nil)
    ((not (equal (matrix-cols mat_a) (matrix-cols mat_b))) nil)
    ((not (equal (matrix-data mat_a) (matrix-data mat_b))) nil)
    (t t)))

;;; MATRIX CREATION ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro empty-matrix-macro (rows cols &rest default)
  `(flet ((generate-empty-matrix (,rows ,cols)
       (loop for i from 1 to ,rows
          collect ,@default)))

    (make-matrix :rows ,rows
                 :cols ,cols
                 :data (generate-empty-matrix ,rows ,cols))))

;;; Create a matrix of size [rows; cols] filled with NIL values.
(defun empty-matrix (rows cols &optional default)
  (empty-matrix-macro rows cols (make-list cols :initial-element nil)))

;;; Create an empty matrix of the same size as given matrix.
(defun empty-matrix-like (mat)
  (empty-matrix (matrix-rows mat) (matrix-cols mat)))

;;; Generate and initialize matrix with a given value.
(defun initialize-matrix (rows cols val)
  (empty-matrix-macro rows cols (make-list cols :initial-element val)))

;;; Generate matrix filled with random normal values.
;;; mean 0
;;; std  1
;;; TODO: keep here?
(defun rand-norm-matrix (rows cols)
  (empty-matrix-macro rows cols (make-list-rand-normal cols)))

;;; Control if all rows have the same number of columns.
;;; TODO where to assign?
(defmacro valid-matrix (row_lengths)
  `(if (= 0
          (apply #'+ (mapcar #'(lambda (x) (- x (car ,row_lengths))) ,row_lengths)))
     t
     nil))

;;; Create a matrix structure from given data (lists of lists).
;;; TODO should we use valid-matrix?
(defun matrix-from-data (data)
  (if (not (valid-matrix (mapcar #'length data)))
      (error 'matrix-error :text "Length of matrix rows is not consistent."))

  (let ((rows (length data))
        (cols (car (mapcar #'length data))))

    (make-matrix :rows rows
                 :cols cols
                 :data data)))

;;; Adds additional layer (list) around given data in order to be able to create
;;; matrix from this data.
(defun matrix-from-data-peel (data)
  (matrix-from-data (list data)))

;;; Removes layer (access the first item of list) from given matrix.
(defun matrix-data-peel (data)
  (car (matrix-data data)))

;;; SINGLE MATRIX OPERATIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Auxiliary function. Shouldn't be employed by itself? TODO move to somewhere else?
(defun transpose-list (lst)
  (apply #'mapcar (cons #'list lst)))

;;; Transpose matrix.
(defun transpose (mat)
  (let ((rows (matrix-rows mat))
        (cols (matrix-cols mat))
        (data (matrix-data mat)))

    (make-matrix :rows cols
                 :cols rows
                 :data (transpose-list data))))

;;; Return n-th row from given matrix.
(defun nth-row (row mat)
  (list (nth row (matrix-data mat))))

;;; Return n-th col from given matrix.
(defmacro nth-col (col mat)
  `(mapcar #'(lambda (x) (list (nth ,col x))) (matrix-data ,mat)))

;;; Access subset of rows from given matrix.
;;; idx >= from AND idx <= to
(defun [] (from to mat)
  (let ((mat-list (matrix-data mat))
        (to-verif (min to (1- (matrix-rows mat)))))

    (matrix-from-data
      (mapcar #'(lambda (idx) (nth idx mat-list))
            (iota (1+ (- to-verif from)) from)))))

;;; Samuel Edwin Ward
;;; http://stackoverflow.com/questions/9444885/common-lisp-how-to-return-a-list-without-the-nth-element-of-a-given-list
;;; Remove the nth element from list.
;;; TODO move to some simplier library?
(defun remove-nth (n list)
  (declare
    (type (integer 0) n)
    (type list list))
  (if (or (zerop n) (null list))
    (cdr list)
    (cons (car list) (remove-nth (1- n) (cdr list)))))

;;; Remove column from given matrix.
;;; Create new matrix.
(defun remove-col (col mat)
  (matrix-from-data
    (mapcar #'(lambda (x) (remove-nth col x)) (matrix-data mat))))

;;; Removes row from given matrix.
;;; Create a new matrix.
(defun remove-row (row mat)
  (matrix-from-data (remove-nth row (matrix-data mat))))

;;; Remove row from matrix composed of lists.
(defun remove-row-list-matrix (row list-mat)
  (remove-nth row list-mat))

;;; Append constant number at the beginning of each row of given matrix.
(defun prefix-const-val (val mat)
  (matrix-from-data (mapcar #'(lambda (x) (push val x)) (matrix-data mat))))

;;; Auxiliary function for MATRIX-INDICES.
(defun matrix-indices-rec (rows cols orig_cols)
  (if (eq rows 0)
    nil
    (cons (cons (1- rows) (1- cols))
          (if (eq (1- cols) 0)
            (matrix-indices-rec (1- rows) orig_cols orig_cols)
            (matrix-indices-rec rows (1- cols) orig_cols)))))

;;; Generate list of matrix indices from given matrix dimensions.
(defun matrix-indices (rows cols)
  (matrix-indices-rec rows cols cols))

;;; Basic sigmoid function.
;;; Accept only single number.
;;; TODO move to only mathematical library for common functions?
(defun sigmoid-base (num)
  (/ 1 (+ 1 (exp (- num)))))

;;; Calculate sigmoid of each value in given matrix.
(defun sigmoid (mat)
  (matrix-from-data
    (mapcar #'(lambda (x) (mapcar #'(lambda (y) (sigmoid-base y)) x))
            (matrix-data mat))))

;;; Derivation of sigmoid function.
(defun sigmoid-prime (mat)
  (let ((s (sigmoid mat)))
    (matrix-mult s (value-matrix-subtract 1 s))))

;;; Randomly shuffle rows of matrix.
;;; TODO unit test?
(defun shuffle-rows (mat)
  (let ((n-rows (matrix-rows mat))
        (mat-list (matrix-data mat))
        (mat-shuffled NIL)
        (rand-idx 0))

  (dotimes (i n-rows)
    (setf rand-idx (random (- n-rows i)))
    (push (nth rand-idx mat-list) mat-shuffled)
    (setf mat-list (remove-row-list-matrix rand-idx mat-list)))

  (matrix-from-data mat-shuffled)))

;;; MATRIX MULTIPLICATION ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Element-wise product of two vectors.
;;; Assume correct vector dimensions.
;;; TODO move to some library with simplier functions?
(defun vec-mult (vec1 vec2)
  (apply #'+ (mapcar #'* vec1 vec2)))

;;; Calculate the new values for one cell of result matrix.
;;; Auxiliary function for DOT-REC.
(defun dot-cell-calc (mat_out row_idx col_idx row_vec col_vec)
  (setf (nth col_idx (nth row_idx (matrix-data mat_out)))
        (vec-mult (car row_vec)
                   (car (transpose-list col_vec))))

  mat_out)

;;; Auxiliary function for DOT function.
(defun dot-rec (mat_out mat_l mat_r mat_idxs)
  (if (eq (car mat_idxs) nil)
    mat_out
    (let* ((row_idx (caar mat_idxs))
           (col_idx (cdar mat_idxs))
           (row_vec (nth-row row_idx mat_l))
           (col_vec (nth-col col_idx mat_r)))

    (dot-rec (dot-cell-calc mat_out row_idx col_idx row_vec col_vec) mat_l mat_r (cdr mat_idxs)))))

;;; Control matrix dot product validity.
(defun valid-dot-op (mat_l mat_r)
  (if (not (= (matrix-cols mat_l)
              (matrix-rows mat_r)))
    (error 'matrix-error :text "Matrices cannot be multiplied. Dimensions do not fit.")))

;;; Matrix multiplication of two matrices.
;;; TODO should we slow down performance with VALID-DOT-OP?
(defun dot (mat_l mat_r)
  (valid-dot-op mat_l mat_r)

  (let* ((rows (matrix-rows mat_l))
         (cols (matrix-cols mat_r))
         (mat_out (empty-matrix rows cols))
         (mat_idxs (matrix-indices rows cols)))

  (setf mat_out (dot-rec mat_out mat_l mat_r mat_idxs))

  mat_out))

;;; ELEMENT-WISE OPERATIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TODO simplify/shorten definition of those functions, too much repetition

;;; Auxiliary function for ADD, SUBTRACT and MATRIX-MULT.
(defun element-wise-op (lst_l lst_r op)
  (mapcar #'(lambda (x y) (mapcar op x y)) lst_l lst_r))

;;; Element-wise add for matrices.
(defun add (mat_l mat_r)
  (matrix-from-data
    (element-wise-op (matrix-data mat_l) (matrix-data mat_r) #'+)))

;;; Elementwise subtract for matrices.
(defun subtract (mat_l mat_r)
  (matrix-from-data
    (element-wise-op (matrix-data mat_l) (matrix-data mat_r) #'-)))

;;; Elementwise matrix multiplication.
(defun matrix-mult (mat_l mat_r)
  (matrix-from-data
    (element-wise-op (matrix-data mat_l) (matrix-data mat_r) #'*)))

(defun matrix-div (mat_l mat_r)
  (matrix-from-data
    (element-wise-op (matrix-data mat_l) (matrix-data mat_r) #'/)))

;;; MATRIX & VALUE OPERATIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Subtract matrix value from given matrix.
(defun value-matrix-subtract (val mat)
  (matrix-from-data
    (mapcar #'(lambda (x) (mapcar #'(lambda (y) (- val y)) x))
            (matrix-data mat))))

;;; Add constant value to given matrix.
;;; TODO merge base of value-matrix based functions
(defun add-value (val mat)
  (matrix-from-data
    (mapcar #'(lambda (x) (mapcar #'(lambda (y) (+ y val)) x))
            (matrix-data mat))))

;;; Multiply matrix with a given value.
(defun multiply (val mat)
  (matrix-from-data
    (mapcar #'(lambda (x) (mapcar #'(lambda (y) (* y val)) x))
            (matrix-data mat))))

;;; Compute power using given exponent at each cell of matrix.
(defun power (val mat)
  (matrix-from-data
    (mapcar #'(lambda (x) (mapcar #'(lambda (y) (expt y val)) x))
            (matrix-data mat))))

;;; MATRIX-ROW/COL OPERATIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Auxiliary function for ELWISE-MAT-ROW-OP.
(defun elwise-row-row-op (lst_row_l lst_row_r op)
  (mapcar #'(lambda (x y) (apply op (list x y))) lst_row_l lst_row_r))

;;; Auxiliary function for SUBTRACT-ROW.
(defun elwise-mat-row-op (lst_mat lst_row op)
  (mapcar #'(lambda (x) (elwise-row-row-op x lst_row op)) lst_mat))

;;; Element-wise subtract values of given row from all rows in matrix.
(defun subtract-row (mat row)
  (matrix-from-data
    (elwise-mat-row-op (matrix-data mat) (car (matrix-data row)) #'-)))

;;; Subtract value from specified column in matrix.
(defun subtract-val-col (val col mat)
  (matrix-from-data
    (mapcar #'(lambda (row) (let ((row-val (nth col row)))
                                   (setf (nth col row) (- row-val val))
                                   row))
              (matrix-data mat))))

;;; Perform aggregating operation on each row of matrix.
;;; Auxiliary function for SUM-ROWS.
(defun rows-op (mat_lst op)
  (mapcar #'(lambda (x) (list (apply op x))) mat_lst))

;;; Sum values at each row of matrix.
(defun sum-rows (mat)
  (matrix-from-data
    (rows-op (matrix-data mat) #'+)))

;;; Sum values at each column of matrix.
(defun sum-cols (mat)
  (transpose (matrix-from-data
               (rows-op (matrix-data (transpose mat)) #'+))))

;;; Compute mean for all columns.
(defun mean-cols (mat)
  (let ((num-rows (matrix-rows mat)))

    (matrix-from-data (transpose-list
      (mapcar #'(lambda (column)
                  (list (/ (apply #'+ column) num-rows)))
              (transpose-list (matrix-data mat)))))))

;;; Compute standard deviation for all columns.
;;; TODO unit tests
(defun std-cols (mat mean)
  (let ((mat-list (matrix-data mat))
        (mean-list (matrix-data-peel mean))
        (row-num (matrix-rows mat))
        (std-list (make-list (matrix-cols mat) :initial-element 0)))

    (mapcar #'(lambda (row)
      (setf std-list
        (mapcar #'+ std-list ;; add to temporary value
                (mapcar #'(lambda (val) (expt val 2)) ;; compute squares
                                     (mapcar #'- row mean-list)))))
      mat-list)

    (matrix-from-data-peel (mapcar #'sqrt
      (mapcar #'(lambda (val) (/ val row-num)) std-list)))))

;;; Sorts column vector and return indices of sorted vector.
(defun arg-sort-col-mat (col_mat)
  (let* ((vec (matrix-data col_mat))
         (idxs (iota (length vec)))
         (join-vec-idxs (mapcar #'(lambda (x y) (cons (car x) y)) vec idxs)))

    (mapcar #'(lambda (x) (cdr x)) (stable-sort join-vec-idxs #'< :key #'car))))

;;; Find the smallest value in specific column of a given matrix.
;;; TODO merge common function for nth-col-max and nth-col-min
(defun nth-col-max (col mat)
  (maximum (nth col (transpose-list (matrix-data mat)))))

;;; Find the largest value in specific column of a given matrix.
(defun nth-col-min (col mat)
  (minimum (nth col (transpose-list (matrix-data mat)))))
