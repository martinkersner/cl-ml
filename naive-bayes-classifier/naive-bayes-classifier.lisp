;;;; Martin Kersner, m.kersner@gmail.com
;;;; 2017/05/29 
;;;;
;;;; Naive Bayes (binary) Classifier

(defclass naive-bayes-classifier ()
  ((vocabulary :accessor get-vocabulary
               :initform '())))

(defgeneric get-default-dataset (nbc)
  (:documentation ""))

(defmethod get-default-dataset ((nbc naive-bayes-classifier))
  (let ((X '((my dog has flea problems help please)
             (maybe not take him to dog park stupid)
             (my dalmation is so cute I love him)
             (stop posting stupid worthless garbage)
             (mr licks ate my steak how to stop him)
             (quit buying worthless dog food stupid)))
        (y (matrix-from-data '((0 1 0 1 0 1)))))

    (values X y)))

(defgeneric make-vocabulary (nbc documents)
  (:documentation ""))

(defmethod make-vocabulary ((nbc naive-bayes-classifier) documents)
  (let ((vocabulary '()))

    (mapcar #'(lambda (d)
                (mapcar #'(lambda (w)
                            (if (not (member w vocabulary))
                              (push w vocabulary)))
                            d)) documents)

    vocabulary))

(defgeneric doc2vec (nbc document vocabulary)
  (:documentation ""))

(defmethod doc2vec ((nbc naive-bayes-classifier) document vocabulary)
  (let* ((index 0)
         (vec (make-list (length vocabulary) :initial-element 0)))

    (mapcar #'(lambda (w) (progn (setf index (position w vocabulary))
                                 (if index
                                    (setf (nth index vec) 1)
                                    (print "Word is not in vocabulary"))))
            document)
    
    vec))

(defgeneric docs2vec (nbc documents vocabulary)
  (:documentation ""))

(defmethod docs2vec ((nbc naive-bayes-classifier) documents vocabulary)
  (matrix-from-data (mapcar #'(lambda (d) (doc2vec nbc d vocabulary)) documents))
  )

(defgeneric fit (nbc X y &optional params)
  (:documentation "Fit the model according to the given training data. Only binary classes are expected."))

(defmethod fit ((nbc naive-bayes-classifier) X y &optional params)
  (let* ((vocabulary (make-vocabulary nbc X))
         (X-mat (docs2vec nbc X vocabulary))
         (rows (matrix-rows X-mat))
         (cols (matrix-cols X-mat))
         (p1 (/ (sum y) rows))
         (p0nom (ones cols))
         (p1nom (ones cols))
         (p0denom 2)
         (p1denom 2))

    (mapcar #'(lambda (f c) (if (= c 1)
                              (progn
                                (setf p1nom (+mm p1nom (matrix-from-data-peel f)))
                                (setf p1denom (+ p1denom (apply #'+ f))))
                              (progn
                                (setf p0nom (+mm p0nom (matrix-from-data-peel f)))
                                (setf p0denom (+ p0denom (apply #'+ f))))))
            (matrix-data X-mat)
            (first (matrix-data y)))

    (values (apply-mat (/mv p0nom p0denom) log)
            (apply-mat (/mv p1nom p1denom) log)
            p1)))

(defgeneric predict (nbc X &optional params)
  (:documentation "Predict class labels for samples in X."))

(defmethod predict ((nbc naive-bayes-classifier) X &optional params)
  )
