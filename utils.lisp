;;;; Martin Kersner, m.kersner@gmail.com
;;;; 2016/04/26

(load "math")

(defun list-to-col-vector-rec (arr idx lst)
  (if (eq lst nil)
    arr
    (progn (setf (aref arr idx 0) (car lst))
           (list-to-col-vector-rec arr (1+ idx) (cdr lst)))))

(defun list-to-col-vector (lst)
  (let* ((n (length lst))
         (arr (make-array (list n 1))))

    (list-to-col-vector-rec arr 0 lst)))

;;; Samuel Edwin Ward
;;; http://stackoverflow.com/questions/9444885/common-lisp-how-to-return-a-list-without-the-nth-element-of-a-given-list
(defun remove-nth (n list)
  (declare
    (type (integer 0) n)
    (type list list))
  (if (or (zerop n) (null list))
    (cdr list)
    (cons (car list) (remove-nth (1- n) (cdr list)))))

;;; Update row of matrix A with values in list.
(defun update-row (A row_idx lst)
  (let ((m (array-dimension A 1)))

  (dotimes (col_idx m)
    (setf (aref A row_idx col_idx) (nth col_idx lst)))
    
   A))

;;; Append constant number at the beginning of each row.
;;; Create new matrix with appended values. 
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

;;; Remove column at position pos from given matrix A.
(defun remove-col (A pos)
  (let* ((n (array-dimension A 0))
         (m (array-dimension A 1))
         (B (make-array (list n (1- m)))))

    (dotimes (row_idx n)
      (setf tmp_vals (remove-nth pos (map 'list #'identity (array-row-slice A row_idx))))
      (setf B (update-row B row_idx tmp_vals)))

  B))
