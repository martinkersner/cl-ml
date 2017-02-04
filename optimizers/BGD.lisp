;;;; Martin Kersner, m.kersner@gmail.com
;;;; 2016/10/14 
;;;;
;;;; Batch Gradient Descent

(defun BGD-optimizer (clf X-data y-labels &key num-epoch lr)
  (let* ((d (matrix-cols X-data))
         (W (initialize-matrix d 1 1))
         (y-pred nil)
         
         ; default parameters
         (num-epoch (if (not num-epoch) 1 num-epoch))
         (lr        (if (not lr) 0.01 lr)))

    (dotimes (i num-epoch)
      (setf y-pred (funcall clf X-data W))

      (setf W (-mm W 
                   (dot (*mv (transpose X-data) lr)
                        (-mm y-pred y-labels)))))

  W))
