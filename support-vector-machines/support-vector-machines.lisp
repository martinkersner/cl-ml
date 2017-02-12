;;;; Martin Kersner, m.kersner@gmail.com
;;;; 2017/01/24 
;;;;
;;;; Support Vector Machines 

(defclass support-vector-machines ()
  ((X      :accessor get-X)
   (y      :accessor get-y)

   (alphas :accessor get-alphas)
   (K      :accessor get-K)

   (C      :accessor get-C)
   (toler  :accessor get-toler)
   (cache  :accessor get-error-cache)

   (W      :accessor get-W)
   (b      :accessor get-b
           :initform 0)))

(defgeneric fit (svm X y &optional params)
  (:documentation "Fit the SVM model according to the given training data."))

(defmethod fit ((svm support-vector-machines) X y &optional params)
  (setf (get-X svm) X)
  (prepare-kernel svm params)
  (smo svm X y params)
  (calculate-weights svm))

(defgeneric predict (svm X &optional params)
  (:documentation "Perform classification on samples in X."))

(defmethod predict ((svm support-vector-machines) X &optional params)
  (+mv
    (dot X (get-W svm) :keep t)
    (get-b svm)))

(defun calculate-weights (svm)
  (let* ((X (get-X svm))
         (y (get-y svm))
         (data-rows (matrix-rows X))
         (data-cols (matrix-cols X))
         (alphas (get-alphas svm)))

    (setf (get-W svm) (zeros (list data-cols 1)))

    (mapcar #'(lambda (idx)
                (setf (get-W svm)
                      (+mm (get-W svm)
                           (*mv (transpose ([] X :row idx))
                                (* ([] alphas :row idx)
                                   ([] y      :row idx))))))
            (iota data-rows))))

(defun select-random-j (i m)
  (nth
    (random (1- m))
    (delete i (iota m))))

(defun clip-alpha (a H L)
  (cond ((> a H) H)
        ((> L a) L)
        (t a)))

(defun calc-Ek (svm k)
  (let* ((X (get-X svm))
         (y (get-y svm))
         (fXk (+ (get-b svm)
                 (dot (transpose (*mm (get-alphas svm) y))
                      ([] (get-K svm) :col k)))))

  (- fXk ([] y :row k))))

(defun select-j (svm i Ei)
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

(defun update-Ek (svm k)
  (let ((Ek (calc-Ek svm k)))
    (update-error-cache svm k Ek)))

(defun update-error-cache (svm idx val)
  (setf ([] (get-error-cache svm) :row idx)
        (list (list 1 val))))

(defun inner-L (svm i)
  (let (
        (X (get-X svm))
        (y (get-y svm))
        (toler (get-toler svm))
        (C (get-C svm))
        (K (get-K svm))

        (Ei (calc-Ek svm i))

        (j nil)
        (Ej nil)
        (alpha-i-old)
        (alpha-j-old)
        (L nil)
        (H nil)
        (eta nil)
        (b1 nil)
        (b2 nil)
        (pair-changed 0))

    (tagbody
    (if (or (and
              (< (* ([] y :row i) Ei) (- toler))
              (< ([] (get-alphas svm) :row i) C))
            (and
              (> (* ([] y :row i) Ei) toler)
              (> ([] (get-alphas svm) :row i) 0)))

            (progn
              (multiple-value-setq (j Ej) (select-j svm i Ei))

              (setf alpha-i-old ([] (get-alphas svm) :row i))
              (setf alpha-j-old ([] (get-alphas svm) :row j))

              (if (not (= ([] y :row i) ([] y :row j)))
                (progn
                  (setf L (max 0 (- ([] (get-alphas svm) :row j) ([] (get-alphas svm) :row i))))
                  (setf H (min C (+ C (- ([] (get-alphas svm) :row j) ([] (get-alphas svm) :row i))))))
                (progn
                  (setf L (max 0 (- (+ ([] (get-alphas svm) :row j) ([] (get-alphas svm) :row i)) C)))
                  (setf H (min C (+ ([] (get-alphas svm) :row j) ([] (get-alphas svm) :row i))))))

              (if (= L H)
                (go continue))

              (setf eta
                (-
                  (* 2 ([] K :row i :col j))
                  ([] K :row i :col i)
                  ([] K :row j :col j)))

              (if (>= eta 0)
                (go continue))

              (setf ([] (get-alphas svm) :row j)
                (- ([] (get-alphas svm) :row j) (/ (* ([] y :row j)
                                         (- Ei Ej))
                                      eta)))
              (setf ([] (get-alphas svm) :row j)
                (clip-alpha ([] (get-alphas svm) :row j) H L))

              (update-Ek svm j)

              (if (< (abs (- ([] (get-alphas svm) :row j)
                             alpha-j-old))
                     0.00001)
                (go continue))

              (setf ([] (get-alphas svm) :row i)  ; TODO inc
                (+ ([] (get-alphas svm) :row i)
                   (* ([] y :row j)
                      ([] y :row i)
                      (- alpha-j-old ([] (get-alphas svm) :row j)))))

              (update-Ek svm i)

              (setf b1
                (- (get-b svm)
                   Ei
                   (* ([] y :row i) (- ([] (get-alphas svm) :row i) alpha-i-old) ([] K :row i :col i))
                   (* ([] y :row j) (- ([] (get-alphas svm) :row j) alpha-j-old) ([] K :row i :col j))))

              (setf b2
                (- (get-b svm)
                   Ej
                   (* ([] y :row i) (- ([] (get-alphas svm) :row i) alpha-i-old) ([] K :row i :col j))
                   (* ([] y :row j) (- ([] (get-alphas svm) :row j) alpha-j-old) ([] K :row j :col j))))

              (cond ((and (< 0 ([] (get-alphas svm) :row i)) (> C ([] (get-alphas svm) :row i)))
                     (setf (get-b svm) b1))
                    ((and (< 0 ([] (get-alphas svm) :row j)) (> C ([] (get-alphas svm) :row j)))
                     (setf (get-b svm) b2))
                    (t
                      (setf (get-b svm) (/ (+ b1 b2) 2))))

              (setf pair-changed 1))

              (setf pair-changed 0))

    continue
    (setf pair-changed 0))

    pair-changed))

(defun smo (svm X y &optional params)
  (let* ((C       (gethash 'C       params))
         (toler   (gethash 'toler   params))
         (maxiter (gethash 'maxiter params))

         (m (matrix-rows X))
         (iter 0)
         (entire-set t)
         (alpha-pairs-changed 0)
         (alphas-shape (list m 1))
         (alphas (zeros alphas-shape)))

    (setf (get-X svm) X)
    (setf (get-y svm) y)
    (setf (get-C svm) C)
    (setf (get-toler svm) toler)
    (setf (get-alphas svm) alphas)
    (setf (get-error-cache svm) (zeros (list m 2)))

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
               (mapcar #'(lambda (idx)
                           (setf alpha-pairs-changed
                                 (+ alpha-pairs-changed (inner-L svm idx))))
                       (iota m))
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

(defun kernel-trans (svm A params)
  (let* ((kernel-type (gethash 'type params))

         (X (get-X svm))
         (rows (matrix-rows X))
         (K (zeros (list rows 1)))
         (delta-row nil))

    (cond ((eq kernel-type 'linear)
           (setf K
                 (dot X (transpose A))))

          ((eq kernel-type 'rbf)
           (progn
             (mapcar #'(lambda (idx)
                       (progn
                         (setf delta-row (-mm ([] X :row idx) A))
                         (setf ([] K :row idx) (dot
                                                 delta-row
                                                 (transpose delta-row)))))
                   (iota rows))
             (setf K
                   (apply-mat
                     (/mv K (- (expt (gethash 'delta params) 2)))
                     (lambda (x) (exp x)))))))

    (matrix-data K)))

(defun prepare-kernel (svm params)
  (let* ((rows  (matrix-rows (get-X svm)))
         (shape (list rows rows)))

    (setf (get-K svm) (zeros shape))

    (mapcar #'(lambda (idx)
                (setf ([] (get-K svm) :col idx)
                      (kernel-trans svm ([] (get-X svm) :row idx) params)))
            (iota rows))))
