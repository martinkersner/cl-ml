;;;; Martin Kersner, m.kersner@gmail.com
;;;; 2017/05/29 
;;;;
;;;; Naive Bayes (binary) Classifier Example

(defparameter *nbc* (make-instance 'naive-bayes-classifier))
(multiple-value-setq (X y) (get-default-dataset *nbc*))

(fit *nbc* X y)

(defparameter *X-test* '((my dog has steak)
                         (your dog is stupid)))

(print (predict *nbc* X-test))
