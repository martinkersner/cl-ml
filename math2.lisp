;;;; Martin Kersner, m.kersner@gmail.com
;;;; 2016/04/27 
;;;;
;;;; Math 2.0
;;;; Mathematical operations, mainly with matrices.
;;;; Matrices are represented by lists.

;;; TODO 
;;; matrix dot product
;;; matrix element-wise multiplication
;;; smart operations (add vector to all rows or columns of matrix)

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
                 :data (apply #'mapcar (cons #'list data)))))

(defun nth-row (row matrix)
  (list (nth row (matrix-data matrix))))

(defmacro nth-col (col matrix)
  `(mapcar #'(lambda (x) (list (nth ,col x))) (matrix-data ,matrix)))


(defun matrix-indices (rows cols)
  (matrix-indices-rec (rows cols cols)))

(defun matrix-indices-rec (rows cols orig_cols)
  (if (eq rows 0)
    nil
    (cons (cons (1- rows) (1- cols))
          (if (eq (1- cols) 0)
            (matrix-indices-rec (1- rows) orig_cols orig_cols)
            (matrix-indices rows-rec (1- cols) orig_cols)))))

;;; Control matrix dot product validity.
(defun valid-dot-op (mat_l mat_r)
  (if (not (= (matrix-col mat_l)
              (matrix-row mat_r)))
    (error 'matrix-error :text "Matrices cannot be multiplied. Dimensions do not fit.")))

;(defun dot (mat_l mat_r)
;  (valid-dot-op (mat_l mat_r))
;
;  (let ((rows (matrix-rows mat_l))
;        (cols (matrix-cos mat_r))
;        (mat_idxs (matrix-indices rows cols)))
;
;  (matrix-from-data (dot-rec mat_l mat_r))
;  )
;  )
;
;(defun dot-rec (mat_res mat_l mat_r indixes)
;  (if (eq (car idxs) nil)
;    mat_res
;    (let* ((row_idx (caar idxs))
;           (col_idx (cdar idxs))
;           (row_vec (map 'list #'identity (array-row-slice mat_left row_idx)))
;           (col_vec (array-col-slice mat_right col_idx)))
;
;    (dot-rec (mul_vec_of_mat mat_res row_idx col_idx row_vec col_vec) mat_left mat_right (cdr idxs)))))
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(defun dot (mat_left mat_right)
;  (let* ((rows (array-dimension mat_left 0))
;         (cols (array-dimension mat_right 1))
;         (idxs (create-matrix-indices rows cols cols)))
;
;  (dot-rec (make-array (list rows cols)) mat_left mat_right idxs)))
;
;(defun dot-rec (mat_res mat_left mat_right idxs)
;  (if (eq (car idxs) nil)
;    mat_res
;    (let* ((row_idx (caar idxs))
;           (col_idx (cdar idxs))
;           (row_vec (map 'list #'identity (array-row-slice mat_left row_idx)))
;           (col_vec (array-col-slice mat_right col_idx)))
;
;    (dot-rec (mul_vec_of_mat mat_res row_idx col_idx row_vec col_vec) mat_left mat_right (cdr idxs)))))
