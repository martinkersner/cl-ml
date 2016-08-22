;;;; Martin Kersner, m.kersner@gmail.com
;;;; 2016/08/12
;;;;
;;;; Linear regression for 1D data.

(in-package :lispml)

(defun train-linear-regression-1D (data-lst labels-lst)
  (let* ((mean-x  (mean data-lst))
         (mean-y  (mean labels-lst))
         (var-x   (var data-lst :ddof 1))
         (cov-x-y (cov data-lst labels-lst))

         (beta    (/ cov-x-y var-x))
         (alpha   (- mean-y (* beta mean-x))))

    (list alpha beta)))
