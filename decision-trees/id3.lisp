;;;; Martin Kersner, m.kersner@gmail.com
;;;; 2016/08/24
;;;;
;;;; Decision Tree classifier using Iterative Dichotomiser 3 (id3).

(in-package :lispml)

(defclass id3-dt ()
  ;((weights :accessor get-weights)))
  ())

(defgeneric fit (dt X y)
  (:documentation ""))

(defmethod fit ((dt id3-dt) X y)
  )

(defgeneric predict (dt X)
  (:documentation ""))

(defmethod predict ((dt id3-dt) X)
  )

(defgeneric score (dt X y)
  (:documentation ""))

(defmethod score ((dt id3-dt) X y)
  )
