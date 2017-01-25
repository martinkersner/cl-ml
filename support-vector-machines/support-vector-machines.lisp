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
  (smo-simple svm X y params))

(defgeneric smo-simple (svm X y &optional params)
  (:documentation ""))

(defmethod smo-simple ((svm support-vector-machines) X y &optional params)
  (let* ((C       (gethash 'C       params))
         (toler   (gethash 'toler   params))
         (maxiter (gethash 'maxiter params))
         (m (matrix-rows X))
         (n (matrix-cols X))
         (fXi nil)
         (Ei nil)
         (b 0)
         (alphas (empty-matrix m 1 0)))

    (loop for i from 0 below 1 collect
          (progn
            (setf fXi
                  (dot (transpose (matrix-mult alphas y))
                       (dot X (transpose ([] i i X)))))
            (setf Ei (- fXi ([] i i y)))

            ;(if (or (and
                      ;(< (* ([] i i y) Ei) (- toler))
                      ;(< ([] i i alphas) C))
                     ;(and
                      ;(> (* ([] i i y) Ei) toler)
                      ;(> ([] i i alphas) 0))
                     ;)
              ;(progn
                ;)
              ;)
    ))))
