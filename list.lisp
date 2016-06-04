;;;; Martin Kersner, m.kersner@gmail.com
;;;; 2016/06/04

(defun range (start end &optional (step 1) (lst NIL))
  (if (<= start end)
    (range (+ start step) end step (append lst (list start)))
    lst))
