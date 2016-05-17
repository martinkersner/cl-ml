;;;; Martin Kersner, m.kersner@gmail.com
;;;; 2016/04/27 
;;;;
;;;; Math 2.0
;;;; Mathematical operations, mainly with matrices. Matrices are represented by 
;;;; lists. Most operations have strict prototype in order to avoid additional
;;;; within functions.

;;; TODO 
;;; smart operations (add vector to all rows or columns of matrix)
;;; nth-row nth-col base functions for returning complete matrices
;;; rewrite remove-nth function and move to file with list operations
;;; unit test for sigmoid functions
;;; merge similar code from multiply, power, value-matrix-subtract functions together
;;; more inspiration from numpy

(load "random")

(defstruct matrix rows cols data)

(define-condition matrix-error (error)
  ((text :initarg :text :reader text)))

;;; Simple example of matrix creation
(defvar *my-matrix* (make-matrix :rows 2 
                                 :cols 2 
                                 :data '((1 2)
                                         (3 4))))

;;; Compare rows and columns of two matrices and their data.
(defun compare-matrix (mat_a mat_b)
  (cond 
    ((not (equal (matrix-rows mat_a) (matrix-rows mat_b))) nil)
    ((not (equal (matrix-cols mat_a) (matrix-cols mat_b))) nil)
    ((not (equal (matrix-data mat_a) (matrix-data mat_b))) nil)
    (t t)))

(defmacro empty-matrix-macro (rows cols &rest default)
  `(flet ((generate-empty-matrix (,rows ,cols)
       (loop for i from 1 to ,rows
          collect ,@default)))

    (make-matrix :rows ,rows
                 :cols ,cols
                 :data (generate-empty-matrix ,rows ,cols))))

;;; Create a matrix of size rowsxcols filled with nil values.
(defun empty-matrix (rows cols &optional default)
  (empty-matrix-macro rows cols (make-list cols :initial-element nil)))

;;; Create an empty matrix of the same size as given matrix.
(defun empty-matrix-like (mat)
  (empty-matrix (matrix-rows mat) (matrix-cols mat)))

;;; Generate and initialize matrix with given value.
(defun initialize-matrix (rows cols val)
  (empty-matrix-macro rows cols (make-list cols :initial-element val)))

;;; Generate matrix filled with random normal values.
;;; mean 0
;;; std 1
(defun rand-norm-matrix (rows cols)
  (empty-matrix-macro rows cols (make-list-rand-normal cols)))

;;; Control if all rows have the same number of columns.
(defmacro valid-matrix (row_lengths)
  `(if (= 0
          (apply #'+ (mapcar #'(lambda (x) (- x (car ,row_lengths))) ,row_lengths)))
     t
     nil))

;;; Create a matrix structure from given data (lists of lists).
(defun matrix-from-data (data)
  (if (not (valid-matrix (mapcar #'length data)))
      (error 'matrix-error :text "Length of matrix rows is not consistent."))

  (let ((rows (length data))
        (cols (car (mapcar #'length data))))

    (make-matrix :rows rows
                 :cols cols
                 :data data)))

;;; Perform transpose of matrix or vector.
(defun transpose (mat)
  (let ((rows (matrix-rows mat))
        (cols (matrix-cols mat))
        (data (matrix-data mat)))

    (make-matrix :rows cols
                 :cols rows
                 :data (transpose-list data))))
                 ;:data (apply #'mapcar (cons #'list data)))))

(defun transpose-list (lst)
  (apply #'mapcar (cons #'list lst)))

(defun nth-row (row matrix)
  (list (nth row (matrix-data matrix))))

(defmacro nth-col (col matrix)
  `(mapcar #'(lambda (x) (list (nth ,col x))) (matrix-data ,matrix)))

(defun matrix-indices (rows cols)
  (matrix-indices-rec rows cols cols))

(defun matrix-indices-rec (rows cols orig_cols)
  (if (eq rows 0)
    nil
    (cons (cons (1- rows) (1- cols))
          (if (eq (1- cols) 0)
            (matrix-indices-rec (1- rows) orig_cols orig_cols)
            (matrix-indices-rec rows (1- cols) orig_cols)))))

;;; Control matrix dot product validity.
(defun valid-dot-op (mat_l mat_r)
  (if (not (= (matrix-cols mat_l)
              (matrix-rows mat_r)))
    (error 'matrix-error :text "Matrices cannot be multiplied. Dimensions do not fit.")))

;;; Dot product of two matrices.
(defun dot (mat_l mat_r)
  (valid-dot-op mat_l mat_r)

  (let* ((rows (matrix-rows mat_l))
         (cols (matrix-cols mat_r))
         (mat_out (empty-matrix rows cols))
         (mat_idxs (matrix-indices rows cols)))

  (setf mat_out (dot-rec mat_out mat_l mat_r mat_idxs))

  mat_out))

(defun dot-rec (mat_out mat_l mat_r mat_idxs)
  (if (eq (car mat_idxs) nil)
    mat_out
    (let* ((row_idx (caar mat_idxs))
           (col_idx (cdar mat_idxs))
           (row_vec (nth-row row_idx mat_l))
           (col_vec (nth-col col_idx mat_r)))

    (dot-rec (dot-cell-calc mat_out row_idx col_idx row_vec col_vec) mat_l mat_r (cdr mat_idxs)))))

;;; Calculate the new values for one cell of result matrix.
(defun dot-cell-calc (mat_out row_idx col_idx row_vec col_vec)
  (setf (nth col_idx (nth row_idx (matrix-data mat_out)))
        (vec-mult2 (car row_vec)
                   (car (transpose-list col_vec))))

  mat_out)

;;; Element-wise product of two vectors.
;;; Assume correct vector dimensions.
(defun vec-mult2 (vec1 vec2)
  (apply #'+ (mapcar #'* vec1 vec2)))

;;; Samuel Edwin Ward
;;; http://stackoverflow.com/questions/9444885/common-lisp-how-to-return-a-list-without-the-nth-element-of-a-given-list
;;; Remove the nth element from list.
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

;;; Append constant number at the beginning of each row of given matrix.
(defun prefix-const-val (val mat)
  (matrix-from-data (mapcar #'(lambda (x) (push val x)) (matrix-data mat))))

;;; Basic sigmoid function.
;;; Accept only single number.
(defun sigmoid-base (num)
  (/ 1 (+ 1 (exp (- num)))))

;;; Sigmoid function accept matrix as an argument.
(defun sigmoid (mat)
  (matrix-from-data
    (mapcar #'(lambda (x) (mapcar #'(lambda (y) (sigmoid-base y)) x))
            (matrix-data mat))))

;;; Derivation of sigmoid function.
;;; Accepts only matrix arguments.
(defun sigmoid-prime (mat)
  (let ((s (sigmoid mat)))
    (matrix-mult s (value-matrix-subtract 1 s))))

;;; Elementwise add for vectors and matrices.
(defun add (mat_l mat_r)
  (matrix-from-data
    (element-wise-op (matrix-data mat_l) (matrix-data mat_r) #'+)))

;;; Elementwise subtract for vectors and matrices.
(defun subtract (mat_l mat_r)
  (matrix-from-data
    (element-wise-op (matrix-data mat_l) (matrix-data mat_r) #'-)))

;;; Elementwise matrix/vector multiplication.
(defun matrix-mult (mat_l mat_r)
  (matrix-from-data
    (element-wise-op (matrix-data mat_l) (matrix-data mat_r) #'*)))

(defun element-wise-op (lst_l lst_r op)
  (mapcar #'(lambda (x y) (mapcar op x y)) lst_l lst_r))

;;; Subtract matrix value from given matrix/vector.
(defun value-matrix-subtract (val mat)
  (matrix-from-data
    (mapcar #'(lambda (x) (mapcar #'(lambda (y) (- val y)) x))
            (matrix-data mat))))

;;; Multiply matrix/vector with a given value.
(defun multiply (val mat)
  (matrix-from-data
    (mapcar #'(lambda (x) (mapcar #'(lambda (y) (* y val)) x))
            (matrix-data mat))))

;;; Compute power using given exponent at each cell of matrix.
(defun power (val mat)
  (matrix-from-data
    (mapcar #'(lambda (x) (mapcar #'(lambda (y) (expt y val)) x))
            (matrix-data mat))))

;;; Element-wise subtract values of given row from all rows in matrix.
(defun subtract-row (mat row)
  (matrix-from-data
    (elwise-mat-row-op (matrix-data mat) (car (matrix-data row)) #'-)))

(defun elwise-mat-row-op (lst_mat lst_row op)
  (mapcar #'(lambda (x) (elwise-row-row-op x lst_row op)) lst_mat))

(defun elwise-row-row-op (lst_row_l lst_row_r op)
  (mapcar #'(lambda (x y) (apply op (list x y))) lst_row_l lst_row_r))

;;; Sum values at each row of matrix.
(defun sum-rows (mat)
  (matrix-from-data
    (rows-op (matrix-data mat) #'+)))

;;; Perform aggregating operation on each row of matrix.
(defun rows-op (mat_lst op)
  (mapcar #'(lambda (x) (list (apply op x))) mat_lst))

;;; Sorts column vector and return indices of sorted vector.
(defun arg-sort-col-mat (col_mat)
  (let* ((vec (matrix-data col_mat))
         (idxs (iota (length vec)))
         (join-vec-idxs (mapcar #'(lambda (x y) (cons (car x) y)) vec idxs)))

    (mapcar #'(lambda (x) (cdr x)) (stable-sort join-vec-idxs #'< :key #'car))))

;;; LISTS

;;; http://aima.cs.berkeley.edu/lisp/utilities/utilities.lisp
;;; Return a list of n consecutive integers, by default starting at 0.
(defun iota (n &optional (start-at 0))
  (if (<= n 0) nil (cons start-at (iota (- n 1) (+ start-at 1)))))

;;; Return the last element of given list.
(defun last-elem (lst)
  (car (last lst)))

;;; Return given element of list even if the index is negative number.
;(defun nth-pos-neg (idx lst)
;  (let* ((l (length lst))
;         (start-idx (if (< idx 0) l 0))
;         (real-index (+ start-idx idx)))
;
;    (nth real-index lst)))
