;;;; Martin Kersner, m.kersner@gmail.com
;;;; 2016/10/14 
;;;;
;;;; Stochastic Gradient Descent
;;;;
;;;; TODO terminating condition when converged 
;;;; TODO function for decreasing learning rate

(defun SGD-optimizer (clf X-data y-labels &key num-epoch lr)
  (let* ((n (matrix-rows X-data))
         (d (matrix-cols X-data))
         (W (initialize-matrix d 1 1))
         (rand-idx nil)
         (idx nil)
         (y-pred nil)
         (y-correct nil)

         ; default parameters
         (num-epoch (if (not num-epoch) 1 num-epoch))
         (lr        (if (not lr) 0.01 lr)))

    (dotimes (i num-epoch)
      (let ((data-idx (iota n)))

      (dotimes(j n)
        ;(setf lr (+ 0.01 (/ 4 (+ 1.0 i j)))) ; why 4?
        (setf rand-idx (random (length data-idx)))
        (setf idx (nth rand-idx data-idx))

        (setf X-vec     (matrix-from-data (nth-row idx X-data)))
        (setf y-correct (matrix-from-data (nth-row idx y-labels)))

        ; prediction with the latest weights
        (setf y-pred    (funcall clf X-vec W))

        (setf W (-mm W
                     (dot (*mv (transpose X-vec) lr)
                          (-mm y-pred y-correct))))

        (setf data-idx (remove-nth rand-idx data-idx)))))

  W))
