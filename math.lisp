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
(defun mean (lst &optional (len-par NIL))
  (let ((len-lst (if len-par
                   len-par
                   (length lst))))

    (/ (apply #'+ lst) len-lst)))

;;; Compute variance of a given list.
(defun var (lst)
  (let* ((len-lst (length lst))
         (mean-lst (mean lst len-lst)))
    (/
      (apply #'+
             (mapcar #'(lambda (x) (expt (- x mean-lst) 2)) lst))
      len-lst))) ; TODO use (- len-lst 1) instead?
