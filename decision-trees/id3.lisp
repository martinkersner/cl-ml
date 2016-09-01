;;;; Martin Kersner, m.kersner@gmail.com
;;;; 2016/08/24
;;;;
;;;; Decision Tree classifier using Iterative Dichotomiser 3 (id3).

(in-package :lispml)

(defclass id3-dt ()
  ;((weights :accessor get-weights)))
  ())

(defun split-node (x-lst y-lst)
  (let ((left-idx  nil)
        (right-idx nil)
        (threshold (mean (remove-duplicates y-lst :test 'equal))))

    (mapcar #'(lambda (idx val) (if (< val threshold)
                                  (push idx left-idx)
                                  (push idx right-idx)))

            (iota (length x-lst))
            x-lst)

    (list left-idx right-idx)
  ))

(defun extract-rows (idxs lst)
  (mapcar #'(lambda (idx) (nth idx lst)) idxs))

(defun fit-id3-rec (parent-entropy X-lst y-lst)
  (let ((max-val    0) ; TODO change!
        (max-ixd    0)

        (tmp-information-gain 0)
        (split-idxs  nil)
        (left-idxs  nil)
        (right-idxs nil)
        (feature-idx-split 0)
        (current-entropy (entropy y-lst))
        )

    (mapcar #'(lambda (idx x-veclst) (progn
                                       (setf split-idxs (split-node x-veclst y-lst))
                                       (setf tmp-information-gain (information-gain
                                                                    parent-entropy
                                                                    (extract-rows (car split-idxs) y-lst)
                                                                    (extract-rows (cadr split-idxs) y-lst)))

                                       (if (> max-val tmp-information-gain)
                                         (progn
                                           (setf left-idxs (car split-idxs))
                                           (setf right-idxs (cadr split-idxs))
                                           (setf feature-idx-split idx)))))
            (iota (length X-lst))
            X-lst)

    (list feature-idx-split

          (if (> (length left-idxs) 0)
            (fit-id3-rec current-entropy
                         (transpose-list (extract-rows left-idxs (transpose-list X-lst)))
                         (extract-rows left-idxs y-lst))
            NIL)

          (if (> (length right-idxs) 0)
            (fit-id3-rec current-entropy
                         (transpose-list (extract-rows right-idxs (transpose-list X-lst)))
                         (extract-rows right-idxs y-lst))
            NIL))
  ))

(defgeneric fit (dt X y)
  (:documentation ""))

(defmethod fit ((dt id3-dt) X y)
  (let ((X-lst (matrix-data (transpose X)))
        (y-lst (matrix-data-peel y))
        (tree nil))

    (fit-id3-rec 1 X-lst y-lst)
  ))

(defgeneric predict (dt X)
  (:documentation ""))

(defmethod predict ((dt id3-dt) X)
  )

(defgeneric score (dt X y)
  (:documentation ""))

(defmethod score ((dt id3-dt) X y)
  )
