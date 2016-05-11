;;;; Martin Kersner, m.kersner@gmail.com
;;;; 2016/05/11 
;;;;
;;;; Functions for generating pseudorandom numbers.
;;;;
;;;; TODO
;;;; unit tests?

(defun rand-normal-spec (mean std)
  (+ mean (* std 
             (sqrt (* -2 (log (random 1.0)))) 
             (cos (* 2 pi (random 1.0))))))

(defun rand-normal ()
  (rand-normal-spec 0 1.0))

;;; Generate list of random normal values.
(defun make-list-rand-normal (len)
  (loop for i from 0 below len collect (rand-normal)))
