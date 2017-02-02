;;;; Martin Kersner, m.kersner@gmail.com
;;;; 2016/05/11 
;;;;
;;;; Neural Networks 
;;;; Inspired by https://github.com/mnielsen/neural-networks-and-deep-learning/blob/master/src/network.py
;;;
;;; TODO
;;; change algorithm in order to be able to access data which are stored in row-like style
;;; control of object initialization 
;;; create a simple dataset

(in-package :cl-ml)

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

(defgeneric fit (nn X y &optional params)
  (:documentation "Train neural network."))

(defmethod fit ((nn neural-network) X y &optional params)
  (let ((num-epoch (gethash 'num-epoch params))
        (lr        (gethash 'lr        params))
        (mini-batch-size (gethash 'mini-batch-size params)))

  (SGD nn
       X y ; training data
       num-epoch mini-batch-size lr
       X y))) ; test data

(defgeneric predict (nn X &optional params)
  (:documentation "Predict using neural network."))

; TODO
(defmethod predict ((nn neural-network) X &optional params)
  )

(defgeneric feed-forward (nn input)
  (:documentation "Computation of feed forward step within neural network."))

(defmethod feed-forward ((nn neural-network) input)
  (let ((a input))

    (mapcar #'(lambda (w b)
                (setf a (sigmoid (add (dot w a) b))))

            (weights nn) (biases nn))

  a))

;;; Individual records are expected to be in row-like formatting.
(defmethod SGD ((nn neural-network) train-data train-labels epochs mini-batch-size lr &optional test-data  test-labels)
  (let* ((n (matrix-rows train-data))
         (mini-batch-range (range 0 n mini-batch-size)))

    (dotimes (j epochs)
      (setf rand-idx (randomize-list (iota n)))
      (setf train-data-rand   (shuffle-rows-spec train-data   rand-idx))
      (setf train-labels-rand (shuffle-rows-spec train-labels rand-idx))

      (mapcar #'(lambda (idx)
                  (progn
                    (setf data-mini-batch   (create-mini-batch train-data-rand   mini-batch-size idx))
                    (setf labels-mini-batch (create-mini-batch train-labels-rand mini-batch-size idx))
                    (update-mini-batch nn data-mini-batch labels-mini-batch lr)))

              mini-batch-range)

      (if test-data
        (evaluate nn test-data test-labels)))))

;;; Access and return specified mini-batch of data.
(defun create-mini-batch (data mini-batch-size idx)
  ([] idx (+ idx (- mini-batch-size 1)) data))

(defmethod update-mini-batch ((nn neural-network) data-mini-batch labels-mini-batch lr)
  (let ((grad-b (mapcar #'zero-matrix-like (biases  nn)))
        (grad-w (mapcar #'zero-matrix-like (weights nn)))
        (modif-lr (* lr (matrix-rows data-mini-batch)))) ; is it correct?

    (mapcar #'(lambda (x y) (progn
                              (multiple-value-setq (delta-grad-b delta-grad-w) (backpropagation nn x y))
                              (setf grad-b
                                    (mapcar #'(lambda (base delta) (add base delta)) grad-b delta-grad-b))
                              (setf grad-w
                                    (mapcar #'(lambda (base delta) (add base delta)) grad-w delta-grad-w))))

            (matrix-data data-mini-batch) (matrix-data labels-mini-batch))

    (setf (slot-value nn 'weights)
          (mapcar #'(lambda (w g-w)
                      (subtract w (multiply modif-lr g-w)))
                  (weights nn) grad-w))

    (setf (slot-value nn 'biases)
          (mapcar #'(lambda (b g-b)
                      (subtract b (multiply modif-lr g-b)))
                  (biases nn) grad-b))))

(defmethod backpropagation ((nn neural-network) x y)
  (let ((grad-b (mapcar #'empty-matrix-like (biases nn)))
        (grad-w (mapcar #'empty-matrix-like (weights nn)))
        (a (matrix-from-data (transpose-list (list x))))
        (a-hist (list x))
        (z-hist nil))

    (mapcar #'(lambda (w b) (progn
                              (setf z (add (dot w a) b))
                              (setf z-hist (append z-hist (list z)))
                              (setf a (sigmoid z))
                              (setf a-hist (append a-hist (list a)))))

      (weights nn) (biases nn))

    (setf delta (matrix-mult (subtract (last-elem a-hist) (matrix-from-data-peel y))
                             (sigmoid-prime (last-elem z-hist))))

    (setf (nth-pos-neg -1 grad-b) delta)
    (setf (nth-pos-neg -1 grad-w) (dot delta (transpose (nth-pos-neg -2 a-hist))))

    (loop for l from 2 to (1- (num-layers nn))
      do ((lambda () (progn
          ;; TODO shorten expression
          (setf delta (multiply (caar (matrix-data (sigmoid-prime (nth-pos-neg (- l) z-hist))))
                                (dot (transpose (nth-pos-neg (1+ (- l)) grad-w)) delta)))
          (setf (nth-pos-neg (- l) grad-b) delta)
          (setf (nth-pos-neg (- l) grad-w) (dot delta (matrix-from-data-peel (nth-pos-neg (1- (- l)) a-hist))))
    ))))

  (values grad-b grad-w)))

(defmethod evaluate ((nn neural-network) test-x test-y)
  (let ((correct 0))

    (mapcar #'(lambda (x y) (progn
                              ;(print (matrix-data-peel (feed-forward nn (transpose (matrix-from-data-peel x))))) ; TODO delete

                              (setf tmp (car (matrix-data-peel (feed-forward nn (transpose (matrix-from-data-peel x))))))

                              (if (= (car y)
                                   ;(maximum-idx (matrix-data-peel (feed-forward nn (transpose (matrix-from-data-peel x))))))
                                   (if (> tmp 0.5)
                                     1
                                     0))
                              (incf correct 1))))
            (matrix-data test-x) (matrix-data test-y))

    (print correct) ; only for debugging purposes
  correct))
