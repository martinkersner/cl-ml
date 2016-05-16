;;;; Martin Kersner, m.kersner@gmail.com
;;;; 2016/05/11 
;;;;
;;;; Neural Networks 
;;;
;;; TODO
;;; control of object initialization 
;;;
;;; How to use?
;;; (matrix-from-data '((8 7)))
;;; (defparameter *nn* (make-instance 'neural-network :nn-dims '(2 3 1)))

(load "math2")

(defclass neural-network ()
  ((nn-dims    :reader nn-dims :initarg :nn-dims)
   (num-layers :reader num-layers)
   (biases     :reader biases)
   (weights    :reader weights)))

(defmethod initialize-instance :after ((nn neural-network) &rest args)
  (let* ((nn-dims (nn-dims nn))
         (front (subseq nn-dims 0 (1- (length nn-dims))))
         (back  (subseq nn-dims 1)))

    (with-slots (num-layers) nn
    (with-slots (nn-dims)    nn
      (setf num-layers (length nn-dims))))

    (with-slots (biases) nn
      (setf biases
        (mapcar #'(lambda (x) (rand-norm-matrix x 1)) back)))

    (with-slots (weights) nn
      (setf weights
        (mapcar #'(lambda (x y) (rand-norm-matrix x y)) back front)))))

(defgeneric feed-forward (nn input)
  (:documentation "Computation of feed forward step within neural network."))

(defmethod feed-forward ((nn neural-network) input)
  (let ((a input))
    (mapcar #'(lambda (w b)
                (setf a (sigmoid (add (dot w a) b))))
    (weights nn) (biases nn))

  a))

(defparameter *input* (matrix-from-data '((8)(7))))
(defparameter *nn* (make-instance 'neural-network :nn-dims '(2 3 1)))
;(setf b (feed-forward *nn* *input*))
