;;;; Martin Kersner, m.kersner@gmail.com
;;;; 2017/07/16 
;;;;
;;;; k-Means 

(defclass k-means ()
  ((k :initarg :k ; number of centroids
      :reader get-k)
   (centroids :accessor get-centroids)))

(defmethod random-init ((km k-means) X)
  (let ((k (get-k km))
        (centroids '()))

    (labels ((random-centroid (range)
               (let ((min-val (car range))
                     (range-val (cadr range)))
                 (+ (* range-val (random 1.0)) min-val)))

             (compute-range (lst)
               (multiple-value-setq (min-val min-idx)
                 (minimum lst))
               (multiple-value-setq (max-val max-idx)
                 (maximum lst))

               (list min-val (- max-val min-val))
             ))

      (setf ranges
        (mapcar #'(lambda (features) (compute-range features))
                (matrix-data (transpose X))))

      (loop for centroid-idx from 1 to k do
        (push
          (mapcar #'(lambda (r) (random-centroid r)) ranges)
          centroids))

    
      centroids)))

(defgeneric fit (km X y &optional params)
  (:documentation ""))

(defmethod fit ((km k-means) X y &optional params)
  (let ((centroids (random-init km X))
        (assignment))

    (labels ((euclidean-dist (vec1 vec2)
               (apply '+
                      (mapcar #'(lambda (v1 v2) (expt (- v1 v2) 2))
                              vec1 vec2)))

             (find-closest (vec centroids)
               (multiple-value-setq (min-val min-idx)
                 (minimum
                   (mapcar #'(lambda (c)
                               (euclidean-dist vec c))
                           centroids)))

               min-idx)

             (assign (centroids X)
               (mapcar #'(lambda (vec)
                             (find-closest vec centroids))
                       X))

             ;(update ())
             )


      (setf assignment
            (assign centroids (matrix-data X)))
      ;(update X)
      ;(setf (get-centroids km) centroids)
    )))

(defgeneric predict (km data &optional params)
  (:documentation ""))

(defmethod predict ((km k-means) data &optional params)
  )
