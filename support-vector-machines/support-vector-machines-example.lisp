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
    2))

;;; TRAINING
(defparameter *params*
  (generate-params '((C       0.6)
                     (toler   0.002)
                     (maxiter 40))))
(fit *svm* train-data train-labels *params*)

;;; plot training data
(defparameter *sv* (find-support-vectors *svm*))
(multiple-value-setq (*x-min* *y-min* *x-max* *y-max*) (compute-hyperplane *svm*))
(defparameter *predictions* (predict *svm* train-data))

(defparameter *fig* (make-instance 'figure
                                   :nokey t))
(xlabel *fig* "feature 1")
(ylabel *fig* "feature 2")
(arrow *fig* *x-min* *y-min* *x-max* *y-max* :nohead t)
(scatter *fig* '((1 1)))
;(scatter *fig* (matrix-data (vstack train-data train-labels))
(scatter *fig* (matrix-data (vstack train-data *predictions*))
         :palette t
         :pt 7
         :ps 2)
(scatter *fig* (matrix-data *sv*)
         :plot-type 'replot
         :palette t
         :with 'circles
         :fill t
         :solid-border t
         :lt 2)
(show *fig*)

(print
  (predict *svm* (matrix-from-data '((3.542485 1.977398)      ; -1
                                     (7.551510 -1.580030))))) ; +1
