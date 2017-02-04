;;;; Martin Kersner, m.kersner@gmail.com
;;;; 2016/10/16
;;;;
;;;; L2 norm of row-oriented data
;;;
;;; TODO recognize orientation of matrices (row vs col subtraction)

; Compute L2 distance between X1 and X2.
; Number of rows of X1 should be the same or larger than X2.
(defun L2 (X1 X2 &key (precise NIL))
  (if (= (matrix-rows X2) (matrix-rows X1))
    (L2-single-row    X1 X2 :precise precise)
    (L2-multiple-rows X1 X2 :precise precise)))

; Single rows
(defun L2-single-row (X1 X2 &key precise)
  (if precise
    (L2-single-row-precise X1 X2)
    (L2-single-row-approx  X1 X2)))

; TODO unit tests
(defun L2-single-row-precise (X1 X2)
  (expt (sum (power 2 (-mm X1 X2))) 0.5))

; TODO unit tests
(defun L2-single-row-approx (X1 X2)
  (sum (power 2 (-mm X1 X2))))

; Multiple rows
(defun L2-multiple-rows (X1 X2 &key precise)
  (if precise
    (L2-multiple-rows-precise X1 X2)
    (L2-multiple-rows-approx  X1 X2)))

; TODO unit tests
(defun L2-multiple-rows-precise (X1 X2)
  (power 0.5 (sum-rows (power 2 (-mr X1 X2)))))

; TODO unit tests
(defun L2-multiple-rows-approx (X1 X2)
  (sum-rows (power 2 (-mr X1 X2))))
