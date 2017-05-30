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
        (y '(0 1 0 1 0 1)))

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
         ;(vec (ones 1 (length vocabulary)))
         (vec (make-list (length vocabulary) :initial-element 0)))

    (mapcar #'(lambda (w) (progn (setf index (position w vocabulary))
                                 (if index
                                    (setf (nth index vec) 1)
                                    (print "Word is not in vocabulary"))))
            document)
    
    vec))

(defgeneric fit (nbc X y &optional params)
  (:documentation "Fit the model according to the given training data. Only binary classes are expected."))

(defmethod fit ((nbc naive-bayes-classifier) X y &optional params)
  (let* ((vocabulary (make-vocabulary nbc X))
         (X-vec (mapcar #'(lambda (d) (doc2vec nbc d vocabulary)) X))
         (X-size (length X))
         (vec-size (length (car X-vec)))
         (p1 (/ (apply #'+ y) X-size))
         (p0num (make-list vec-size :initial-element 1))
         (p1num (make-list vec-size :initial-element 1))
         (p0denom 2)
         (p1denom 2)
         (p0vect '())
         (p1vect '()))

    (mapcar #'(lambda (f c) (if (= c 1)
                              (progn
                                (setf p1num (mapcar #'(lambda (a b) (+ a b)) p1num f))
                                (setf p1denom (apply #'+ f)))
                              (progn
                                (setf p0num (mapcar #'(lambda (a b) (+ a b)) p0num f))
                                (setf p0denom (apply #'+ f)))))
            X-vec y)

    (setf p0vec (mapcar #'(lambda (p) (/ p p0denom)) p0num))
    (setf p1vec (mapcar #'(lambda (p) (/ p p1denom)) p1num))
    (mapcar #'(lambda (v p) (progn (print v) (print p))) vocabulary p1vec)

    (values p0vec p1vec p1)))

(defgeneric predict (nbc X &optional params)
  (:documentation "Predict class labels for samples in X."))

(defmethod predict ((nbc naive-bayes-classifier) X &optional params)
  )
