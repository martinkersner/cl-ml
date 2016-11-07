;;;; Martin Kersner, m.kesner@gmail.com
;;;; 2016/06/03
;;;; 
;;;; Mathematical operations.

(in-package :lispml)

(setf *math-namespace* '())

(push 'is-even *math-namespace*)
(defun is-even (val)
  (if (eq (mod val 2) 0)
    t
    nil))

(push 'is-odd *math-namespace*)
(defun is-odd (val)
  (if (eq (mod val 2) 0)
    nil
    t))

;;; Compute mean of a given list.
(push 'mean *math-namespace*)
(defun mean (lst &optional (len-par NIL))
  (let ((len-lst (if len-par
                   len-par
                   (length lst))))

    (/ (apply #'+ lst) len-lst)))

;;; Compute variance of a given list.
(push 'var *math-namespace*)
(defun var (lst &key (ddof 0))
  (let* ((len-lst (length lst))
         (mean-lst (mean lst len-lst)))
    (/
      (apply #'+
             (mapcar #'(lambda (x) (expt (- x mean-lst) 2)) lst))
      (- len-lst ddof)))) ; TODO use (- len-lst 1) instead?

;;; Compute covariance of two variables.
;;; Does not control if lists are of the same length.
(push 'cov *math-namespace*)
(defun cov (lst-a lst-b)
  (let* ((len-lst (length lst-a))
         (mean-a (mean lst-a len-lst))
         (mean-b (mean lst-b len-lst)))
    (/
      (apply #'+
             (mapcar #'(lambda (a b) (* (- a mean-a) (- b mean-b))) lst-a lst-b))
      (- len-lst 1))))
