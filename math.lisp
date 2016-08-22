;;;; Martin Kersner, m.kesner@gmail.com
;;;; 2016/06/03
;;;; 
;;;; Mathematical operations.

(in-package :lispml)

(defun is-even (val)
  (if (eq (mod val 2) 0)
    t
    nil))

(defun is-odd (val)
  (if (eq (mod val 2) 0)
    nil
    t))

;;; Compute mean of a given list.
(defun mean (lst)
  (let ((len-lst (length lst)))

    (/ (apply #'+ lst) len-lst)))
