;;;; Martin Kersner, m.kersner@gmail.com
;;;; 2016/10/14 
;;;;
;;;; Batch Gradient Descent
;;;;

(in-package :cl-ml)

(defun BGD-optimizer (clf X-data y-labels &key num-epoch lr)
  (let* ((d (matrix-cols X-data))
         (W (initialize-matrix d 1 1))
         
         ; default parameters
         (num-epoch (if (not num-epoch) 1 num-epoch))
         (lr        (if (not lr) 0.01 lr)))

    (dotimes (i num-epoch)
      (setf y-pred (funcall clf X-data W))

      (setf W (subtract W 
                   (dot (multiply lr (transpose X-data)) 
                          (subtract y-pred y-labels)))))

  W))
