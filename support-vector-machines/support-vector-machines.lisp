;;;; Martin Kersner, m.kersner@gmail.com
;;;; 2017/01/24 
;;;;
;;;; Support Vector Machines 

(defclass support-vector-machines ()
  ((b      :accessor get-b
           :initform 0)
   (alphas :accessor get-alphas)
   (X      :accessor get-X)
   (y      :accessor get-y)
   (C      :accessor get-C)
   (toler  :accessor get-toler)
   ; error cache
   (cache :accessor get-error-cache)))

(defgeneric fit (svm X y &optional params)
  (:documentation ""))

(defmethod fit ((svm support-vector-machines) X y &optional params)
  (smo svm X y params))
  ;(smo-simple svm X y params))

(defun select-random-j (i m)
  (nth
    (random (1- m))
    (delete i (iota m))))

(defun clip-alpha (a H L)
  (cond ((> a H) H)
        ((> L a) L)
        (t a)))

(defgeneric smo-simple (svm X y &optional params)
  (:documentation ""))

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

;;; optimized SMO
(defgeneric calc-Ek (svm k)
  (:documentation ""))

(defmethod calc-Ek ((svm support-vector-machines) k)
  (let* ((X (get-X svm))
         (y (get-y svm))
         (fXk (+ (get-b svm)
                 (dot (transpose (*mm (get-alphas svm) y))
                      (dot X (transpose ([] X :row k)))))))

  (- fXk ([] y :row k))))

(defgeneric select-j (svm i Ei)
  (:documentation ""))

(defmethod select-j ((svm support-vector-machines) i Ei)
  (let ((max-delta-E 0)
        (delta-E nil)
        (data-len (matrix-rows (get-X svm)))
        (Ej 0)
        (Ek nil)
        (j -1))

    (update-error-cache svm i Ei)

    (mapcar #'(lambda (k)
                (if (and (= 1 ([] (get-error-cache svm) :row k :col 0))
                         (not (= k i)))
                  (progn
                    (setf Ek (calc-Ek svm k))
                    (setf delta-E (abs (- Ei Ek)))
                    (if (> delta-E max-delta-E)
                      (progn
                        (setf max-delta-E delta-E)
                        (setf j k)
                        (setf Ej Ek)))
                    )))
            (iota data-len))

    (if (= j -1)
      (setf j (select-random-j i data-len))
      (setf Ej (calc-Ek svm j)))

    (values j Ej)))

(defgeneric update-Ek (svm k)
  (:documentation ""))

(defmethod update-Ek ((svm support-vector-machines) k)
  (let ((Ek (calc-Ek svm k)))
    (update-error-cache svm k Ek)))

(defgeneric update-error-cache (svm idx val)
  (:documentation ""))

(defmethod update-error-cache ((svm support-vector-machines) idx val)
  (setf ([] (get-error-cache svm) :row idx)
        (matrix-from-data (list (list 1 val)))))

(defgeneric inner-L (svm i)
  (:documentation ""))

(defmethod inner-L ((svm support-vector-machines) i)
  (let ((X (get-X svm))
        (y (get-y svm))
        (toler (get-toler svm))
        (alphas (get-alphas svm))
        (C (get-C svm))

        (Ei (calc-Ek svm i))

        (j nil)
        (Ej nil)
        (alpha-i-old)
        (alpha-j-old)
        (L nil)
        (H nil)
        (eta nil)
        (b nil)
        (b1 nil)
        (b2 nil))

    (tagbody
    (if (or (and
              (< (* ([] y :row i) Ei) (- toler))
              (< ([] alphas :row i) C))
            (and
              (> (* ([] y :row i) Ei) toler)
              (> ([] alphas :row i) 0)))

            (progn
              (multiple-value-setq (j Ej) (select-j svm i Ei))

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

              (update-Ek svm j)

              (if (< (abs (- ([] alphas :row j)
                             alpha-j-old))
                     0.00001)
                (go continue))

              (setf ([] alphas :row i)  ; TODO inc
                (+ ([] alphas :row i)
                   (* ([] y :row j)
                      ([] y :row i)
                      (- alpha-j-old ([] alphas :row j)))))

              (update-Ek svm i)

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

              1)

              0)
    continue
    0)))

(defgeneric smo (svm X y &optional params)
  (:documentation ""))

(defmethod smo ((svm support-vector-machines) X y &optional params)
  (let* ((C       (gethash 'C       params))
         (toler   (gethash 'toler   params))
         (maxiter (gethash 'maxiter params))
         (m (matrix-rows X))
         (iter 0)
         (entire-set t)
         (alpha-pairs-changed 0)
         (alphas (empty-matrix m 1 0)))

    (setf (get-X svm) X)
    (setf (get-y svm) y)
    (setf (get-C svm) C)
    (setf (get-toler svm) y)
    (setf (get-alphas svm) alphas)

    (flet ((in-between (val lower higher)
            (cond ((<= val lower) nil)
                  ((>= val higher) nil)
                  (t t))))

    (loop while (and (< iter maxiter)
                     (or (> alpha-pairs-changed 0)
                         entire-set))
      do (progn
           (incf iter)
           (if entire-set
             (progn
               (mapcar #'(lambda (idx) (setf alpha-pairs-changed (+ alpha-pairs-changed (inner-L svm idx)))) (iota m))
               (incf iter))
             (progn
               (mapcar #'(lambda (idx)
                           (if (in-between ([] alphas :row idx) 0 C)
                             (setf alpha-pairs-changed (+ alpha-pairs-changed (inner-L svm idx)))))
                       (iota m))
               (incf iter)
               ))

           (cond (entire-set
                    (setf entire-set nil))
                  ((= alpha-pairs-changed 0)
                   (setf entire-set t)))

           )))))

