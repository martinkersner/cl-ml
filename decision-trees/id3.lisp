;;;; Martin Kersner, m.kersner@gmail.com
;;;; 2016/08/24
;;;;
;;;; Decision Tree classifier using Iterative Dichotomiser 3 (id3).

(in-package :lispml)

(defclass id3-dt ()
  ((tree :accessor get-tree
         :initform nil)))

(defun split-node (x-lst)
  (let ((left-idx  nil)
        (right-idx nil)
        (threshold (mean (remove-duplicates x-lst :test 'equal))))

    (mapcar #'(lambda (idx val) (if (< val threshold)
                                  (push idx left-idx)
                                  (push idx right-idx)))

            (iota (length x-lst))
            x-lst)

    (list left-idx right-idx)))

(defun extract-rows (idxs lst)
  (mapcar #'(lambda (idx) (nth idx lst)) idxs))

(defun fit-id3-rec (parent-entropy X-lst y-lst)
  (let ((max-information-gain 0)
        (tmp-information-gain 0)
        (split-idxs nil)
        (left-idxs  nil)
        (right-idxs nil)
        (feature-idx-split 0)
        (entropy-left  0)
        (entropy-right 0))

    (mapcar #'(lambda (idx x-veclst) (progn
                                       (setf split-idxs (split-node x-veclst))
                                       (setf tmp-information-gain (information-gain
                                                                    parent-entropy
                                                                    (extract-rows (car split-idxs)  y-lst)
                                                                    (extract-rows (cadr split-idxs) y-lst)))

                                       (if (< max-information-gain tmp-information-gain)
                                         (progn
                                           (setf max-information-gain tmp-information-gain)
                                           (setf left-idxs  (car split-idxs))
                                           (setf right-idxs (cadr split-idxs))
                                           (setf feature-idx-split idx)))))
            (iota (length X-lst))
            X-lst)

    ;; Compute entropy for both (left and right) child of the best split according to information gain.
    (setf entropy-left  (entropy (extract-rows left-idxs y-lst)))
    (setf entropy-right (entropy (extract-rows right-idxs y-lst)))

    (list 'id-split feature-idx-split
          'entropy (entropy y-lst)
          'samples (length y-lst)

          (if (and (> (length left-idxs) 1)
                   (> entropy-left 0))
            (fit-id3-rec entropy-left
                         (transpose-list (extract-rows left-idxs (transpose-list X-lst)))
                         (extract-rows left-idxs y-lst))
            (list 'entropy entropy-left
                  'samples (length left-idxs)))

          (if (and (> (length right-idxs) 1)
                   (> entropy-right 0))
            (fit-id3-rec entropy-right
                         (transpose-list (extract-rows right-idxs (transpose-list X-lst)))
                         (extract-rows right-idxs y-lst))
            (list 'entropy entropy-right
                  'samples (length right-idxs))))))

(defgeneric fit (dt X y &optional params)
  (:documentation "Build a decision tree from the training set (X, y)."))

(defmethod fit ((dt id3-dt) X y &optional params)
  (let ((X-lst (matrix-data (transpose X)))
        (y-lst (matrix-data-peel y)))

    (setf (get-tree dt)
      (fit-id3-rec (entropy y-lst) X-lst y-lst))))

(defgeneric predict (dt X &optional params)
  (:documentation ""))

(defmethod predict ((dt id3-dt) X &optional params)
  )

(defgeneric score (dt X y)
  (:documentation ""))

(defmethod score ((dt id3-dt) X y)
  )

(defgeneric print-tree (dt)
  (:documentation "Prints built decision tree."))

(defmethod print-tree ((dt id3-dt))
  (print (get-tree dt)))
