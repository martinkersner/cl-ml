;;;; Martin Kersner, m.kersner@gmail.com
;;;; 2017/01/24 
;;;;
;;;; Support Vector Machines example

(defun find-support-vectors (svm)
  (let* ((X (get-X svm))
         (y (get-y svm))
         (rows (matrix-rows X))
         (alphas (get-alphas svm))
         (support-vectors nil))

    (mapcar #'(lambda (idx)
                (if (not (= ([] alphas :row idx) 0))
                  (setf support-vectors
                        (append support-vectors (list
                                (append (car (matrix-data ([] X :row idx)))
                                        (list ([] y :row idx))))))))
            (iota rows))

    (matrix-from-data support-vectors)))

(defun compute-hyperplane (svm)
  (let* ((X (get-X svm))
         (W (matrix-data (get-w svm)))
         (w1 (caar W))
         (w2 (caadr W))
         (min-x (nth-col-min X 0))
         (max-x (nth-col-max X 0))
         (b (get-b svm)))

    (values
      min-x
      (/ (- (- b) (* min-x w1)) w2)
      max-x
      (/ (- (- b) (* max-x w1)) w2))))

;;; INITIALIZATION
(defparameter *svm* (make-instance 'support-vector-machines))

;;; LOAD DATASET
(multiple-value-setq (train-data train-labels) 
  (load-dataset 
    "datasets/support-vector-machines/dataset.txt"
    ;"datasets/support-vector-machines/rbf1.txt"
    ;"datasets/support-vector-machines/rbf2.txt"
    2))

;;; TRAINING

(defparameter *linear-params*
  (generate-params '((C       0.6)
                     (toler   0.001)
                     (maxiter 40)
                     (type    linear))))

(defparameter *rbf-params*
  (generate-params '((C       200)
                     (toler   0.0001)
                     (maxiter 10000)
                     (type    rbf)
                     (delta   1.3))))

;(fit *svm* train-data train-labels *rbf-params*)
(fit *svm* train-data train-labels *linear-params*)

;;; plot training data
(defparameter *sv* (find-support-vectors *svm*))
(multiple-value-setq (*x-min* *y-min* *x-max* *y-max*) (compute-hyperplane *svm*))
(defparameter *predictions* (predict *svm* train-data))

(defparameter *fig* (make-instance 'figure
                                   :nokey t))
(xlabel *fig* "feature 1")
(ylabel *fig* "feature 2")
(arrow *fig* *x-min* *y-min* *x-max* *y-max* :nohead t)
;(scatter *fig* (matrix-data (vstack train-data train-labels))
(scatter *fig* (matrix-data (vstack train-data *predictions*))
         :palette t
         :pt 7
         :ps 2)
;(scatter *fig* (matrix-data *sv*)
         ;:plot-type 'replot
         ;:palette t
         ;:with 'circles
         ;:fill t
         ;:solid-border t
         ;:lt 2)
(show *fig*)
