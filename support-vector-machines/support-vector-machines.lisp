;;;; Martin Kersner, m.kersner@gmail.com
;;;; 2017/01/24 
;;;;
;;;; Support Vector Machines 

(in-package :lispml)

(defclass support-vector-machines ()
  ((b :accessor get-b)
   (alphas :accessor get-alphas)))

(defgeneric fit (svm X y &optional params)
  (:documentation ""))

(defmethod fit ((svm support-vector-machines) X y &optional params)
  )
