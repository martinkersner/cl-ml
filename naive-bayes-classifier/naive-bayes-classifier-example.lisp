;;;; Martin Kersner, m.kersner@gmail.com
;;;; 2017/05/29 
;;;;
;;;; Naive Bayes Classifier Example

(defparameter *nbc* (make-instance 'naive-bayes-classifier))
(multiple-value-setq (X y) (get-default-dataset *nbc*))
(multiple-value-setq (p0vec p1vec p1) (fit *nbc* X y))

;(print p0vec)
;(print p1vec)
;(print p1)
