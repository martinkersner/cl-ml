;;;; Martin Kersner, m.kersner@gmail.com
;;;; 2016/08/12
;;;;
;;;; Linear regression.
;;;
;;; TODO accept parameter for selecting optimization method

(in-package :lispml)

;;; n-D linear regression
(defclass linear-regression ()
  ((weights :accessor get-weights)))

(defgeneric fit (linreg X y &optional params)
  (:documentation "Fit linear model."))

(defmethod fit ((linreg linear-regression) X y &optional params)
  (let ((X (prefix-const-val 1.0 X))
        (num-epoch (gethash 'num-epoch params))
        (lr        (gethash 'lr        params)))

    (setf (get-weights linreg)
          ;(BGD-optimizer #'(lambda (d w) (dot d w))
          (SGD-optimizer #'(lambda (d w) (dot d w))
                         X y :num-epoch num-epoch :lr lr))))

(defgeneric predict (linreg X &optional params)
  (:documentation "Predict using linear model."))

(defmethod predict ((linreg linear-regression) X &optional params)
  (let ((X (prefix-const-val 1.0 X)))
    (dot X (get-weights linreg))))

(defgeneric score (linreg X y)
  (:documentation "Return the coefficient of determination R^2 of the prediction."))

(defmethod score ((linreg linear-regression) X y)
  (let* ((ss-res (sum-cols (apply-matrix (lambda (val) (expt val 2)) (subtract y (predict linreg X)))))
         (mean-y (nth 0 (matrix-data-peel (mean-cols y))))
         (ss-tot (sum-cols (apply-matrix (lambda (val) (expt (- val mean-y) 2)) y))))

    (- 1 (nth 0 (matrix-data-peel (matrix-div ss-res ss-tot))))))
