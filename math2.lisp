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
  (nth row (matrix-data matrix)))
