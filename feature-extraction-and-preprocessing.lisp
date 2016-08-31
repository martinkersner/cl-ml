;;;; Martin Kersner, m.kersner@gmail.com
;;;; 2016/08/23

(in-package :lispml)

;;; Create single one hot encoding vector.
(defun create-one-hot (len pos)
  (let* ((tmp-one-hot (make-list len :initial-element 0)))
    (setf (nth pos tmp-one-hot) 1)
    
    tmp-one-hot))

;;; Create one hot encoding from given categorical values.
(defun one-hot-encoding (lst)
  (let* ((set-lst (remove-duplicates lst :test 'equal))
         (len-lst (length set-lst))
         (ht (make-hash-table :test 'equal))
         (tmp-one-hot nil))

    (mapcar #'(lambda (idx item) (progn 
                                   (setf tmp-one-hot (create-one-hot len-lst idx)) 
                                   (setf (gethash item ht) tmp-one-hot)))
                (iota len-lst) set-lst)

    ht))

;;; Apply one hot encoding on given categories.
(defun apply-hash-table-on-list (ht lst)
    (mapcar #'(lambda (item) (gethash item ht)) lst))

;;; Create hash table where *keys* are unique items from given list
;;; and *values* are ascending numbers counted from 0.
(defun unique-numbers (lst)
  (let ((set-lst (remove-duplicates lst :test 'equal))
        (ht (make-hash-table :test 'equal)))

    (mapcar #'(lambda (idx item)
                (setf (gethash item ht) idx))
            (iota (length lst))
            set-lst)

    ht))
