;;;; Martin Kersner, m.kersner@gmail.com
;;;; 2017/05/29 
;;;;
;;;; Naive Bayes (binary) Classifier Example

(defparameter *nbc* (make-instance 'naive-bayes-classifier))

(multiple-value-setq (X y) (get-default-dataset *nbc*))

(fit *nbc* X y)
(defparameter *X-test* '((my dog has steak)
                         (your dog is stupid)))
(print (predict *nbc* *X-test*))

;(load "datasets/spam-ham")
;(fit *nbc* *spam-ham-data* *spam-ham-labels*)
;(defparameter *X-test*
  ;'((I am writing to seek your consent to conduct humanitarian projects becos I suffer from advanced cancer that prevents me from realizing my dreams. Doctor informed me that my days are numbered becos of my health is degrading.That is why I want to Send these some of my money to you (EURO 4000000.00) Four million Euro so that you can use it to help the Orphanageshomeless and Widows and 35% for you while you use 65% for the projectif you think you can give these poor children and poor widow a glimmer of hope and joy then let me know  by replying to my private email which is mellissalewis4001@yandex.comThanks for your cooperation and assistance)))
;(print (predict *nbc* *X-test*))
