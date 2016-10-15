;;;; Martin Kersner, m.kersner@gmail.com
;;;; 2016/08/12
;;;;
;;;; Linear regression.

(in-package :lispml)

;;; 1-D linear regression
;;; Deprecated
;(defun train-linear-regression-1D (data-lst labels-lst)
;  (let* ((mean-x  (mean data-lst))
;         (mean-y  (mean labels-lst))
;         (var-x   (var data-lst :ddof 1))
;         (cov-x-y (cov data-lst labels-lst))
;
;         (beta    (/ cov-x-y var-x))
;         (alpha   (- mean-y (* beta mean-x))))
;
;    (list alpha beta)))
;
;(defun predict-linear-regression-1D (alpha beta x)
;  (+ alpha (* beta x)))
;
;(defun test-linear-regression-1D (alpha beta data-lst labels-lst)
;  (let* ((ss-res (apply #'+ (mapcar #'(lambda (x y) (expt (- y (predict-linear-regression-1D alpha beta x)) 2)) data-lst labels-lst)))
;         (mean-y (mean labels-lst))
;         (ss-tot (apply #'+ (mapcar #'(lambda (y) (expt (- y mean-y) 2)) labels-lst))))
;
;    (- 1 (/ ss-res ss-tot))))

;;; n-D linear regression
(defclass linear-regression ()
  ((weights :accessor get-weights)))

(defgeneric fit (linreg X y)
  (:documentation "Fit linear model"))

;;; TODO take num-epoch and lr out of method
(defmethod fit ((linreg linear-regression) X y)
  (let ((X (prefix-const-val 1.0 X))
        (num-epoch 2)
        (lr 0.005))

    (setf (get-weights linreg)
          (SGD-optimizer #'(lambda (d w) (dot d w)) 
                         X y num-epoch lr))))

(defgeneric predict (linreg X)
  (:documentation "Predict using linear model"))

(defmethod predict ((linreg linear-regression) X)
  (let ((X (prefix-const-val 1.0 X)))
    (dot X (get-weights linreg))))

(defgeneric score (linreg X y)
  (:documentation "Return the coefficient of determination R^2 of the prediction."))

(defmethod score ((linreg linear-regression) X y)
  (let* ((ss-res (sum-cols (apply-matrix (lambda (val) (expt val 2)) (subtract y (predict linreg X)))))
         (mean-y (nth 0 (matrix-data-peel (mean-cols y))))
         (ss-tot (sum-cols (apply-matrix (lambda (val) (expt (- val mean-y) 2)) y))))

    (- 1 (nth 0 (matrix-data-peel (matrix-div ss-res ss-tot))))))
