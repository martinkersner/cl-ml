;;;; Martin Kersner, m.kersner@gmail.com
;;;; 2016/05/08 
;;;;
;;;; k-Nearest Neighbors 
;;;
;;; TODO prepare dataset specially for knn
;;; TODO and also include tests for algorithm verification
;;; TODO predict from more feature vectors at once

(in-package :lispml)

(defclass k-nearest-neighbors ()
  ((X :accessor get-X)
   (y :accessor get-y)
   (k :reader   get-k :initform 2))) ; default number of nearest neighbors

(defgeneric fit (knn X y &optional params)
  (:documentation "Fit the model using X as training data and y as target values."))

(defmethod fit ((knn k-nearest-neighbors) X y &optional params)
  (progn (setf (get-X knn) X)
         (setf (get-y knn) y)))

(defgeneric predict (knn X &optional params)
  (:documentation "Predict the class labels for the provided data."))

(defmethod predict ((knn k-nearest-neighbors) X &optional params)
  (let* ((class-count (make-hash-table))
         (labels-lst (matrix-data (get-y knn)))
         (k (read-param params 'k (get-k knn)))
         (k-neighbors (subseq (arg-sort-col-mat
                                (L2 (get-x knn) X))
                              0 k)) ; take only the closest k data points
         (k-labels (mapcar #'(lambda (x) (car (nth x labels-lst))) k-neighbors))
         (final-class 0)
         (max_count 0))

    ;; count number of class occurences
    (mapcar #'(lambda (x)
                (let ((val (gethash x class-count)))
                   (if val
                     (setf (gethash x class-count) (+ 1 val))
                     (setf (gethash x class-count) 1))
                   ))
            k-labels)

    ; find class with the highest occurence
    (maphash #'(lambda (k v)
                  (if (> v max_count)
                      (progn
                        (setf max_count v)
                        (setf final-class k))))
             class-count)

  final-class))
