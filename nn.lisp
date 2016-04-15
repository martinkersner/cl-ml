(defparameter *sizes* '(784 30 10))

(defparameter *a* (make-array '(3 2) 
  :initial-contents '((1 2) (3 4) (5 6))))

(defparameter *b* (make-array '(2 4) 
  :initial-contents '((1 2 3 4) (5 6 7 8))))

; krzysz00
; http://stackoverflow.com/questions/12327237/common-lisp-how-to-access-a-row-of-a-certain-multi-dimension-array
(defun array-row-slice (arr row)
  (make-array (array-dimension arr 1) 
    :displaced-to arr 
    :displaced-index-offset (* row (array-dimension arr 1))))

; TODO finish
(defun array-col-slice (arr col)
  (make-array (array-dimension arr 0) 
    :displaced-to arr 
    :displaced-index-offset (* col (array-dimension arr 0))))
