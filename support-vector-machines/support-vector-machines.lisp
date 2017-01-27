;;;; Martin Kersner, m.kersner@gmail.com
;;;; 2017/01/24 
;;;;
;;;; Support Vector Machines 

(in-package :lispml)

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
         (n (matrix-cols X))
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
         (alphas (empty-matrix m 1 0)))

    (loop while (< iter maxiter) do
      (progn
        (setf alpha-pairs-changed 0)
        (loop for i from 0 below m collect

        (tagbody
        (progn
          (setf fXi
            (+ b (dot (transpose (matrix-mult alphas y))
                      (dot X (transpose ([] i i X))))))

          (setf Ei (- fXi ([] i i y)))

          (if (or (and
                    (< (* ([] i i y) Ei) (- toler))
                    (< ([] i i alphas) C))
                  (and
                    (> (* ([] i i y) Ei) toler)
                    (> ([] i i alphas) 0)))
            (progn
              (setf j (select-random-j i m))
              (setf fXj
                (+ b (dot (transpose (matrix-mult alphas y))
                          (dot X (transpose ([] j j X))))))

              (setf Ej (- fXj ([] j j y)))
              (setf alpha-i-old ([] i i alphas))
              (setf alpha-j-old ([] j j alphas))

              (if (not (= ([] i i y) ([] j j y)))
                (progn
                  (setf L (max 0 (- ([] j j alphas) ([] i i alphas))))
                  (setf H (min C (+ C (- ([] j j alphas) ([] i i alphas))))))
                (progn
                  (setf L (max 0 (- (+ ([] j j alphas) ([] i i alphas)) C)))
                  (setf H (min C (+ ([] j j alphas) ([] i i alphas))))))

              (if (= L H)
                (go continue))

              (setf eta
                (* 2 (-
                  (dot ([] i i X) (transpose ([] j j X)))
                  (dot ([] i i X) (transpose ([] i i X)))
                  (dot ([] j j X) (transpose ([] j j X))))))

              (if (>= eta 0)
                (go continue))

              (setf ([] j j alphas)
                (- ([] j j alphas) (/ (* ([] j j y)
                                         (- Ei Ej))
                                      eta)))
              (setf ([] j j alphas)
                (clip-alpha ([] j j alphas) H L))

              (if (< (abs (- ([] j j alphas)
                             alpha-j-old))
                     0.00001)
                (go continue))

              (setf ([] i i alphas)  ; TODO inc
                (+ ([] i i alphas)
                   (* ([] j j y)
                      ([] i i y)
                      (- alpha-j-old ([] j j alphas)))))

              (setf b1
                (- b
                   Ei
                   (* ([] i i y) (- ([] i i alphas) alpha-i-old) (dot ([] i i X) (transpose ([] i i X))))
                   (* ([] j j y) (- ([] j j alphas) alpha-j-old) (dot ([] i i X) (transpose ([] j j X))))))

              (setf b2
                (- b
                   Ej
                   (* ([] i i y) (- ([] i i alphas) alpha-i-old) (dot ([] i i X) (transpose ([] j j X))))
                   (* ([] j j y) (- ([] j j alphas) alpha-j-old) (dot ([] j j X) (transpose ([] j j X))))))

              (cond ((and (< 0 ([] i i alphas)) (> C ([] i i alphas)))
                     (setf b b1))
                    ((and (< 0 ([] j j alphas)) (> C ([] j j alphas)))
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
