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
   (num_layers   :reader  num_layers)
   (biases       :reader  biases)
   (weights      :reader  weights)))

(defmethod initialize-instance :after (neural-network &key)
  (let* ((network_dims (slot-value neural-network 'network_dims))
         (front (subseq network_dims 0 (1- (length network_dims))))
         (back (subseq network_dims 1)))

    (with-slots (num_layers)   neural-network
    (with-slots (network_dims) neural-network
      (setf num_layers (length network_dims))))

    (with-slots (biases) neural-network
      (setf biases
        (mapcar #'(lambda (x) (rand-norm-matrix x 1)) back)))

    (with-slots (weights) neural-network
      (setf weights
        (mapcar #'(lambda (x y) (rand-norm-matrix x y)) back front)))))
