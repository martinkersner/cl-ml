;;;; Martin Kersner, m.kersner@gmail.com
;;;; 2016/08/24
;;;;
;;;; Example code for Decision Tree classifier using Iterative Dichotomiser 3 (id3).

(defun preprocess-data (data)
  (let ((uniq-lst nil)
        (preprocess-data nil)
        (ht-list '())
        (ht nil))

    (setf preprocessed-data
    (mapcar #'(lambda (lst) (progn
                              (setf uniq-lst (remove-duplicates lst :test 'equal))
                              (if (> (length uniq-lst) 2)
                                (progn
                                  (setf ht (one-hot-encoding uniq-lst T))
                                  (setf ht-list (nconc ht-list (list ht)))
                                  (transpose-list (apply-hash-table-on-list ht lst)))

                                (progn
                                  (setf ht (unique-numbers uniq-lst T))
                                  (setf ht-list (nconc ht-list (list ht)))
                                  (apply-hash-table-on-list ht lst)))))
            (matrix-data (transpose data))))

    (values preprocessed-data ht-list)))

(defun flatten-list (l)
  (cond ((null l) nil)
        ((atom l) (list l))
        (t (loop for a in l appending (flatten-list a)))))

(defun transform-list (lst rows cols)
  (let ((pos-prev 0)
        (pos-next 0))

    (mapcar #'(lambda (row-id) (progn
                                 (setf pos-prev pos-next)
                                 (setf pos-next (+ pos-prev rows))
                                 (subseq lst pos-prev pos-next)))
            (iota cols))))

;;; INITIALIZATION
(defparameter *dt* (make-instance 'id3-dt))

;;; TRAINING
(defparameter *train-dataset-path* "datasets/decision-trees/cat-dog.csv")
(multiple-value-setq (train-data train-labels) (load-dataset *train-dataset-path* 3))

;;; PREPROCESS DATASET
(multiple-value-setq (prep-labels ht-labels)
  (preprocess-data train-labels))

(setf train-labels-processed
  (matrix-from-data prep-labels))

(multiple-value-setq (prep-data ht-data)
  (preprocess-data train-data))

(setf train-data-processed
  (matrix-from-data (transpose-list
                      (transform-list (flatten-list prep-data) 14 5))))

;;; Train model.
(fit *dt* train-data-processed train-labels-processed)

;;; Display trained tree
(print-tree *dt*)

;;;; PREDICTION
;(print
;  (predict *dt* (matrix-from-data '((8 2)(10 10)))))
;
;;;; TESTING
;(defparameter *test-dataset-path* "datasets/linear-regression/pizza-nd-test.csv")
;(multiple-value-setq (test-data test-labels) (load-dataset *test-dataset-path*))
;
;;;; Evaluate model.
;(defparameter *r-squared* (score *dt* test-data test-labels))
;(print *r-squared*)
