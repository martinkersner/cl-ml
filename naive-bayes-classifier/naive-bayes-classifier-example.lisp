;;;; Martin Kersner, m.kersner@gmail.com
;;;; 2017/05/29 
;;;;
;;;; Naive Bayes Classifier Example

(defparameter *nbc* (make-instance 'naive-bayes-classifier))
(multiple-value-setq (X y) (get-default-dataset *nbc*))
(make-vocabulary *nbc* X)
;(print (get-vocabulary *nbc*))
(setf feature-vector (doc2vec *nbc* (nth 2 X)))
