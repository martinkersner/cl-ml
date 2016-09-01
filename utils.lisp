;;;; Martin Kersner, m.kersner@gmail.com
;;;; 2016/09/01
;;;;
;;;; Auxiliary functions.
;;;; Probably some of them are going to be moved to more appropriate places.

(in-package :lispml)

;;; TODO unit tests!
(defun entropy (lst)
  (let ((ht (make-hash-table :test 'equal))
        (key-exist nil)
        (value nil)
        (count-lst nil)
        (tmp-sum nil)
        (tmp-div nil))

    ;; Count number of occurences of all items from original list.
    (mapcar #'(lambda (item) (progn 
                               (multiple-value-setq (value key-exist) (gethash item ht))

                               (if key-exist 
                                 (setf (gethash item ht) (+ value 1))
                                 (setf (gethash item ht) 1))))
            lst)

    ;; Extract only number of occurences.
    (maphash #'(lambda (key value) (push value count-lst)) ht)


    (setf tmp-sum (apply #'+ count-lst))

    ;;; Compute entropy.
    (- (apply #'+
      (mapcar #'(lambda (item) (progn 
                                 (setf tmp-div (/ item tmp-sum))
                                 (* tmp-div (log tmp-div 2)))) 
              count-lst)))
  ))
