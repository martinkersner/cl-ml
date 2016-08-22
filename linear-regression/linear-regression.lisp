;;;; Martin Kersner, m.kersner@gmail.com
;;;; 2016/08/12
;;;;
;;;; Linear regression.

(in-package :lispml)

;;; 1-D linear regression
(defun train-linear-regression-1D (data-lst labels-lst)
  (let* ((mean-x  (mean data-lst))
         (mean-y  (mean labels-lst))
         (var-x   (var data-lst :ddof 1))
         (cov-x-y (cov data-lst labels-lst))

         (beta    (/ cov-x-y var-x))
         (alpha   (- mean-y (* beta mean-x))))

    (list alpha beta)))

(defun predict-linear-regression-1D (alpha beta x)
  (+ alpha (* beta x)))

(defun test-linear-regression-1D (alpha beta data-lst labels-lst)
  (let* ((ss-res (apply #'+ (mapcar #'(lambda (x y) (expt (- y (predict-linear-regression-1D alpha beta x)) 2)) data-lst labels-lst)))
         (mean-y (mean labels-lst))
         (ss-tot (apply #'+ (mapcar #'(lambda (y) (expt (- y mean-y) 2)) labels-lst))))

    (- 1 (/ ss-res ss-tot))))

;;; n-D linear regression
(defun train-linear-regression (X y)
  (let ((X-T (transpose X)))
      (dot (inv (dot X-T X)) (dot X-T y))))

(defun predict-linear-regression (w X)
  (nth 0 (matrix-data-peel (dot X w))))

(defun test-linear-regression (w data-mat labels-mat)
  (let* ((labels-lst (nth 0 (matrix-data (transpose labels-mat))))
         (ss-res (apply #'+ (mapcar #'(lambda (x y) (expt (- y (predict-linear-regression w (matrix-from-data-peel x))) 2)) (matrix-data data-mat) labels-lst)))

         (mean-y (mean labels-lst))
         (ss-tot (apply #'+ (mapcar #'(lambda (y) (expt (- y mean-y) 2)) labels-lst))))

    (- 1 (/ ss-res ss-tot))))
