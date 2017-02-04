;;;; Martin Kersner, m.kersner@gmail.com
;;;; 2016/04/25
;;;;
;;;; Logistic regression
;;;
;;; TODO accept parameter for selecting optimization method

(defclass logistic-regression ()
  ((weights :accessor get-weights)))

(defgeneric fit (logreg X y &optional params)
  (:documentation "Fit the model according to the given training data."))

(defmethod fit ((logreg logistic-regression) X y &optional params)
  (let ((X (prepend-col-val X 1.0))
        (num-epoch (gethash 'num-epoch params))
        (lr        (gethash 'lr        params)))

    (setf (get-weights logreg)
          (SGD-optimizer #'(lambda (d w) (sigmoid (dot d w :keep t)))
                         X y :num-epoch num-epoch :lr lr))))

(defgeneric predict (logreg X &optional params)
  (:documentation "Predict class labels for samples in X."))

(defmethod predict ((logreg logistic-regression) X &optional params)
  (let ((X (prepend-col-val X 1.0)))
    (sigmoid (dot X (get-weights logreg) :keep t))))
