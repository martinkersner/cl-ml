;;;; Martin Kersner, m.kersner@gmail.com
;;;; 2017/07/16 
;;;;
;;;; k-Means example

(defparameter *km*
  (make-instance 'k-means :k 3))

(defparameter *X-train*
  (matrix-from-data '((1 2) (1 4) (1 0)
                      (4 2) (4 4) (4 0))))
(defparameter *X-test*
  (matrix-from-data '((0 0)
                      (4 5))))

(fit *km* *X-train* '())
(predict *km* *X-test*)
