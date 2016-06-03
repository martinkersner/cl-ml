;;;; Martin Kersner, m.kesner@gmail.com
;;;; 2016/06/03
;;;; 
;;;; Mathematical operations.

;;; Find the maximum value and its index in list.
(defun maximum (lst)
  (let ((max-idx 0)
        (max-val (car lst)))

  (mapcar #'(lambda (x y) (if (> x max-val)
                            (progn
                              (setf max-val x)
                              (setf max-idx y))))
          lst (iota (length lst)))

  (values max-val max-idx)))
