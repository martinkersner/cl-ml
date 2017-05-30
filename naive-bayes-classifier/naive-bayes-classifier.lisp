;;;; Martin Kersner, m.kersner@gmail.com
;;;; 2017/05/29 
;;;;
;;;; Naive Bayes (binary) Classifier

(defclass naive-bayes-classifier ()
  ((vocabulary :accessor get-vocabulary
               :initform '())
   (w-cond-c0 :accessor get-w-cond-c0)
   (w-cond-c1 :accessor get-w-cond-c1)
   (p0 :accessor get-p0)
   (p1 :accessor get-p1)))

(defgeneric get-default-dataset (nbc)
  (:documentation "Create default dataset for testing purposes. Returns features and corresponding labels."))

(defmethod get-default-dataset ((nbc naive-bayes-classifier))
  (let ((X '((my dog has flea problems help please)
             (maybe not take him to dog park stupid)
             (your dalmation is so cute I love him)
             (stop posting stupid worthless garbage)
             (mr licks ate my steak how to stop him)
             (quit buying worthless dog food stupid)))
        (y (matrix-from-data '((0 1 0 1 0 1)))))

    (values X y)))

(defgeneric make-vocabulary (nbc documents)
  (:documentation "Create vocabulary out of given documents. Vocabulary contains each word at most once."))

(defmethod make-vocabulary ((nbc naive-bayes-classifier) documents)
  (mapcar #'(lambda (d)
              (mapcar #'(lambda (w)
                          (if (not (member w (get-vocabulary nbc)))
                            (push w (get-vocabulary nbc))))
                          d)) documents)

  (get-vocabulary nbc))

(defgeneric doc2vec (nbc document vocabulary)
  (:documentation "Convert document to feature vector which is represented as a list."))

(defmethod doc2vec ((nbc naive-bayes-classifier) document vocabulary)
  (let* ((index 0)
         (vec (make-list (length vocabulary) :initial-element 0)))

    (mapcar #'(lambda (w) (progn (setf index (position w vocabulary))
                                 (if index
                                    (setf (nth index vec) 1)
                                    (print "Word is not in vocabulary!"))))
            document)
    
    vec))

(defgeneric docs2vec (nbc documents vocabulary)
  (:documentation "Convert list of document to feature vectors which are represented as a matrix."))

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
         (p0num (ones cols))
         (p1num (ones cols))
         (p0denom 2)
         (p1denom 2))

    (mapcar #'(lambda (f c) (if (= c 1)
                              (progn
                                (setf p1num (+mm p1num (matrix-from-data-peel f)))
                                (setf p1denom (+ p1denom (apply #'+ f))))
                              (progn
                                (setf p0num (+mm p0num (matrix-from-data-peel f)))
                                (setf p0denom (+ p0denom (apply #'+ f))))))
            (matrix-data X-mat)
            (first (matrix-data y)))

    (setf (get-w-cond-c0 nbc)
          (apply-mat (/mv p0num p0denom) log))

    (setf (get-w-cond-c1 nbc)
          (apply-mat (/mv p1num p1denom) log))

    (setf (get-p0 nbc)
          (- 1 p1))

    (setf (get-p1 nbc)
          p1)))

(defgeneric predict (nbc X &optional params)
  (:documentation "Predict class labels for samples in X."))

(defmethod predict ((nbc naive-bayes-classifier) X &optional params)
  (labels ((bayes-rule (X w-cond-c p)
                     (+ (sum (*mm X w-cond-c))
                        (log p)))

           (single-predict (X w-cond-c0 w-cond-c1 p0 p1)
                           (if (> (bayes-rule X w-cond-c0 p0)
                                  (bayes-rule X w-cond-c1 p1))
                             0 1))

           (doc2matrix (nbc doc voc)
                       (matrix-from-data-peel
                         (doc2vec nbc doc voc))))

  (let ((vocabulary (get-vocabulary nbc)))

    (matrix-from-data-peel
      (mapcar #'(lambda (d) (single-predict (doc2matrix nbc d vocabulary)
                                            (get-w-cond-c0 nbc)
                                            (get-w-cond-c1 nbc)
                                            (get-p0 nbc)
                                            (get-p1 nbc)))
              X)))))
