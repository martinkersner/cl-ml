;;;; Martin Kersner, m.kersner@gmail.com
;;;; 2017/01/24 
;;;;
;;;; Support Vector Machines 

(defclass support-vector-machines ()
  ((b :accessor get-b)
   (alphas :accessor get-alphas)))

(defgeneric fit (svm X y &optional params)
  (:documentation ""))

(defmethod fit ((svm support-vector-machines) X y &optional params)
  (smo-simple svm X y params))

(defgeneric smo-simple (svm X y &optional params)
  (:documentation ""))

(defun select-random-j (i m)
  (nth
    (random (1- m))
    (delete i (iota m))))

(defun clip-alpha (a H L)
  (cond ((> a H) H)
        ((> L a) L)
        (t a)))

(defmethod smo-simple ((svm support-vector-machines) X y &optional params)
  (let* ((C       (gethash 'C       params))
         (toler   (gethash 'toler   params))
         (maxiter (gethash 'maxiter params))
         (m (matrix-rows X))
         (fXi nil)
         (fXj nil)
         (Ei nil)
         (Ej nil)
         (j nil)
         (iter 0)
         (alpha-i-old nil)
         (alpha-j-old nil)
         (alpha-pairs-changed 0)
         (b 0)
         (b1 0)
         (b2 0)
         (L 0)
         (H 0)
         (eta 0)
         (alphas (empty-matrix m 1 0)))

    (loop while (< iter maxiter) do
      (progn
        (setf alpha-pairs-changed 0)
        (loop for i from 0 below m collect

        (tagbody
        (progn
          (setf fXi
            (+ b (dot (transpose (*mm alphas y))
                      (dot X (transpose ([] X :row i))))))

          (setf Ei (- fXi ([] y :row i)))

          (if (or (and
                    (< (* ([] y :row i) Ei) (- toler))
                    (< ([] alphas :row i) C))
                  (and
                    (> (* ([] y :row i) Ei) toler)
                    (> ([] alphas :row i) 0)))
            (progn
              (setf j (select-random-j i m))
              (setf fXj
                (+ b (dot (transpose (*mm alphas y))
                          (dot X (transpose ([] X :row j))))))

              (setf Ej (- fXj ([] y :row j)))

              (setf alpha-i-old ([] alphas :row i))
              (setf alpha-j-old ([] alphas :row j))

              (if (not (= ([] y :row i) ([] y :row j)))
                (progn
                  (setf L (max 0 (- ([] alphas :row j) ([] alphas :row i))))
                  (setf H (min C (+ C (- ([] alphas :row j) ([] alphas :row i))))))
                (progn
                  (setf L (max 0 (- (+ ([] alphas :row j) ([] alphas :row i)) C)))
                  (setf H (min C (+ ([] alphas :row j) ([] alphas :row i))))))

              (if (= L H)
                (go continue))

              (setf eta
                (* 2 (-
                  (dot ([] X :row i) (transpose ([] X :row j)))
                  (dot ([] X :row i) (transpose ([] X :row i)))
                  (dot ([] X :row j) (transpose ([] X :row j))))))

              (if (>= eta 0)
                (go continue))

              (setf ([] alphas :row j)
                (- ([] alphas :row j) (/ (* ([] y :row j)
                                         (- Ei Ej))
                                      eta)))
              (setf ([] alphas :row j)
                (clip-alpha ([] alphas :row j) H L))

              (if (< (abs (- ([] alphas :row j)
                             alpha-j-old))
                     0.00001)
                (go continue))

              (setf ([] alphas :row i)  ; TODO inc
                (+ ([] alphas :row i)
                   (* ([] y :row j)
                      ([] y :row i)
                      (- alpha-j-old ([] alphas :row j)))))

              (setf b1
                (- b
                   Ei
                   (* ([] y :row i) (- ([] alphas :row i) alpha-i-old) (dot ([] X :row i) (transpose ([] X :row i))))
                   (* ([] y :row j) (- ([] alphas :row j) alpha-j-old) (dot ([] X :row i) (transpose ([] X :row j))))))

              (setf b2
                (- b
                   Ej
                   (* ([] y :row i) (- ([] alphas :row i) alpha-i-old) (dot ([] X :row i) (transpose ([] X :row j))))
                   (* ([] y :row j) (- ([] alphas :row j) alpha-j-old) (dot ([] X :row j) (transpose ([] X :row j))))))

              (cond ((and (< 0 ([] alphas :row i)) (> C ([] alphas :row i)))
                     (setf b b1))
                    ((and (< 0 ([] alphas :row j)) (> C ([] alphas :row j)))
                     (setf b b2))
                    (t
                      (setf b (/ (+ b1 b2) 2))))

              (incf alpha-pairs-changed))

            (if (= alpha-pairs-changed 0)
              (incf iter)
              (setf iter 0))))

      continue))))

    (setf (get-b svm) b)
    (setf (get-alphas svm) alphas)))
