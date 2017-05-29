;;;; Martin Kersner, m.kersner@gmail.com
;;;; 2017/05/29 
;;;;
;;;; Naive Bayes Classifier 

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
  (mapcar #'(lambda (d)
              (mapcar #'(lambda (w)
                          (if (not (member w (get-vocabulary nbc)))
                            (push w (get-vocabulary nbc)))) 
                          d)) documents))

(defgeneric doc2vec (nbc document)
  (:documentation ""))

(defmethod doc2vec ((nbc naive-bayes-classifier) document)
  (let* ((voc (get-vocabulary nbc))
         (index 0)
         (vec (make-list (length voc) :initial-element 0)))

    (mapcar #'(lambda (w) (progn (setf index (position w voc))
                                 (if index
                                    (setf (nth index vec) 1)
                                    (print "Word is not in vocabulary"))))
            document)
    
    vec))

(defgeneric fit (nbc X y &optional params)
  (:documentation "Fit the model according to the given training data."))

(defmethod fit ((nbc naive-bayes-classifier) X y &optional params)
    )

(defgeneric predict (nbc X &optional params)
  (:documentation "Predict class labels for samples in X."))

(defmethod predict ((nbc naive-bayes-classifier) X &optional params)
  )
