;;;; Martin Kersner, m.kersner@gmail.com
;;;; 2017/07/16 
;;;;
;;;; k-Means example

(defparameter *km*
  (make-instance 'k-means :k 2))

(defparameter *X-train*
  (matrix-from-data '((1 2) (1 4) (1 0)
                      (4 2) (4 4) (4 0))))
(defparameter *X-test*
  (matrix-from-data '((0 0)
                      (4 5))))

(fit *km* *X-train* '())
(predict *km* *X-test*)

(setf centroids (get-centroids *km*))

;;; Plot decision boundary.
(defparameter *fig* (make-instance 'figure
                                   :nokey t))
(xlabel *fig* "feature 1")
(ylabel *fig* "feature 2")
(scatter *fig* (transpose-list (append
                 (transpose-list (matrix-data *X-train*))
                 (list (get-labels *km*))))
         :palette t
         :pt 7
         :ps 2
         :with 'points)

(scatter *fig* centroids 
         :plot-type 'replot
         :pt 7
         :ps 2
         :with 'points)
(show *fig*)
