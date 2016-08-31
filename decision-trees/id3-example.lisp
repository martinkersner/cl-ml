;;;; Martin Kersner, m.kersner@gmail.com
;;;; 2016/08/24
;;;;
;;;; Example code for Decision Tree classifier using Iterative Dichotomiser 3 (id3).

(in-package :lispml)

(defun load-dataset (dataset-path)
  (let* (;;; Load data.
        (dataset (matrix-from-data (read-csv dataset-path t)))

        ;;; Data preprocessing.
        (label-col-idx (- (matrix-cols dataset) 1)) ; labels are located in the last column of data matrix
        (data  
          (remove-col label-col-idx dataset))
        (labels 
          (matrix-from-data (nth-col label-col-idx dataset))))

  (values data labels)))

(defun preprocess-data (data)
  (let ((uniq-lst nil)
        (ht-list '())
        (ht nil))

    (mapcar #'(lambda (lst) (progn
                              (setf uniq-lst (remove-duplicates lst :test 'equal))
                              (if (> (length uniq-lst) 2)
                                ;; TODO send a flag that list is already unique
                                (progn
                                  (setf ht (one-hot-encoding uniq-lst))
                                  (setf ht-list (nconc ht-list (list ht)))
                                  (transpose-list (apply-hash-table-on-list ht lst)))

                                (progn
                                  (setf ht (unique-numbers uniq-lst))
                                  (setf ht-list (nconc ht-list (list ht)))
                                  (apply-hash-table-on-list ht lst)))
                              ))

      (matrix-data (transpose data)))))

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
(multiple-value-setq (train-data train-labels) (load-dataset *train-dataset-path*))

;;; PREPROCESS DATASET
(setf train-data-processed
  (matrix-from-data (transpose-list
                      (transform-list (flatten-list (preprocess-data train-data)) 14 5))))

(setf train-labels-processed
  (matrix-from-data (preprocess-data train-labels)))

(print train-data-processed)
(print train-labels-processed)

;;; Train model.
;(fit *dt* train-data train-labels)

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
