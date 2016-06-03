;;;; Martin Kersner, m.kersner@gmail.com
;;;; 2016/05/11 
;;;;
;;;; Neural Networks 
;;;; Inspired by https://github.com/mnielsen/neural-networks-and-deep-learning/blob/master/src/network.py
;;;
;;; TODO
;;; control of object initialization 
;;; create a simple dataset
;;;
;;; How to use?
;;; (matrix-from-data '((8 7)))
;;; (defparameter *nn* (make-instance 'neural-network :nn-dims '(2 3 1)))

(load "matrix")

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

(defmethod backpropagation ((nn neural-network) x y)
  (let ((grad-b (mapcar #'empty-matrix-like (biases nn)))
        (grad-w (mapcar #'empty-matrix-like (weights nn)))
        (a x)
        (a-hist (list x))
        (z-hist nil))

    (mapcar #'(lambda (w b)
                (progn (setf z (add (dot w a) b))
                       (setf z-hist (append z-hist (list z)))
                       (setf a (sigmoid z))
                       (setf a-hist (append a-hist (list a)))))
      (weights nn) (biases nn))

    (setf delta (matrix-mult (subtract (last-elem a-hist) y)
                             (sigmoid-prime (last-elem z-hist))))

    (setf (nth-pos-neg -1 grad-b) delta)
    (setf (nth-pos-neg -1 grad-w) (dot delta (transpose (nth-pos-neg -2 a-hist))))

    (loop for l from 2 to (1- (num-layers nn))
      do ((lambda () (progn
          ;; TODO shorten expression
          (setf delta (multiply (caar (matrix-data (sigmoid-prime (nth-pos-neg (- l) z-hist))))
                                (dot (transpose (nth-pos-neg (1+ (- l)) grad-w)) delta)))
          (setf (nth-pos-neg (- l) grad-b) delta)
          (setf (nth-pos-neg (- l) grad-w) (dot delta (transpose (nth-pos-neg (1- (- l)) a-hist))))
    ))))

  (values grad-b grad-w)
))

;(defmethod evaluate ((nn neural-network) test-x test-y)
;  (let ((correct 0))
;  )
;
;    (mapcar #'(lambda (x) (feed-forward nn x)) test-x)
;)

;;;;;;;;;;;;;;;;;
(defparameter *x* (matrix-from-data '((8)(7))))
(defparameter *y* (matrix-from-data '((1))))
(defparameter *nn* (make-instance 'neural-network :nn-dims '(2 3 1)))
;(setf b (feed-forward *nn* *x*))
(multiple-value-setq (grad-b grad-w) (backpropagation *nn* *x* *y*))
