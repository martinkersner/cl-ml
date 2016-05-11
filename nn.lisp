;;;; Martin Kersner, m.kersner@gmail.com
;;;; 2016/05/11 
;;;;
;;;; Neural Networks 
;;;
;;; TODO
;;; control of object initialization 
;;; stack overflow when loaded twice in the same clisp session
;;; defmethod is locked in clisp
;;;
;;; How to use?
;;; (defparameter *nn* (make-instance 'neural-network :network-dims '(2 3 1)))

(load "math2")

(defclass neural-network ()
  ((network_dims :initarg :network-dims)
   num_layers
   biases
   weights))

(defmethod initialize-instance :after (neural-network &key)
  (let* ((network_dims (slot-value neural-network 'network_dims))
         (front (subseq network_dims 0 (1- (length network_dims))))
         (back (subseq network_dims 1)))

    (setf (slot-value neural-network 'num_layers) 
          (length (slot-value neural-network 'network_dims)))

    (setf (slot-value neural-network 'biases) 
          (mapcar #'(lambda (x) (rand-norm-matrix x 1)) back))

    (setf (slot-value neural-network 'weights) 
          (mapcar #'(lambda (x y) (rand-norm-matrix x y)) back front))
    ))
