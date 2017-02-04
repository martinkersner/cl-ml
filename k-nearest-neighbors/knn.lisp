;;;; Martin Kersner, m.kersner@gmail.com
;;;; 2016/05/08 
;;;;
;;;; k-Nearest Neighbors 
;;;
;;; TODO prepare dataset specially for knn
;;; TODO and also include tests for algorithm verification
;;; TODO predict from more feature vectors at once

(defclass k-nearest-neighbors ()
  ((X :accessor get-X)
   (y :accessor get-y)
   (k :reader   get-k :initform 1))) ; default number of nearest neighbors

(defgeneric fit (knn X y &optional params)
  (:documentation "Fit the model using X as training data and y as target values."))

(defmethod fit ((knn k-nearest-neighbors) X y &optional params)
  (progn (setf (get-X knn) X)
         (setf (get-y knn) y)))

(defgeneric predict (knn X &optional params)
  (:documentation "Predict the class labels for the provided data."))

(defmethod predict ((knn k-nearest-neighbors) X &optional params)
  (let ((labels-lst (matrix-data (get-y knn)))
        (k (read-param params 'k (get-k knn))))

    ;;; Compute distances between training data and given feature vector
    ;;; and return labels of first k closest instances.
    (setf k-labels
          (mapcar #'(lambda (idx)
                      (car (nth idx labels-lst))) ; access label using index

                  (subseq (arg-sort-col-mat ; arg sort L2 differences
                            (L2 (get-x knn) X)) ; compute difference between training data and given feature vector
                          0 k))) ; return first k indices

    ;; Return the class with the most frequent class within k closest neighbors.
    (aggregate-maximum k-labels)))
