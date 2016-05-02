;;;; Martin Kersner, m.kersner@gmail.com
;;;; 2016/04/27 
;;;;
;;;; Math 2.0
;;;; Mathematical operations, mainly with matrices.
;;;; Matrices are represented by lists.

;;; TODO 
;;; matrix element-wise multiplication
;;; smart operations (add vector to all rows or columns of matrix)
;;; nth-row nth-col base functions for returning complete matrices
;;; rewrite remove-nth function and move to file with list operations

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

;;; Create a matrix of size rowsxcols filled with nil values.
(defun empty-matrix (rows cols)
  (flet ((generate-empty-matrix (rows cols) 
       (loop for i from 1 to rows
          collect (make-list cols))))
  
    (make-matrix :rows rows
                 :cols cols
                 :data (generate-empty-matrix rows cols))))


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
