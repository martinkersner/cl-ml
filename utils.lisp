;;;; Martin Kersner, m.kersner@gmail.com
;;;; 2016/09/01
;;;;
;;;; Auxiliary functions.
;;;; Probably some of them are going to be moved to more appropriate places.

(in-package :cl-ml)

;;; Compute entropy of given list.
;;; Created for decision trees algorithm.
;;; TODO unit tests!
(defun entropy (lst)
  (let ((ht (make-hash-table :test 'equal))
        (key-exist nil)
        (value nil)
        (count-lst nil)
        (tmp-sum nil)
        (tmp-div nil))

    ;; Count number of occurences of all items from original list.
    (mapcar #'(lambda (item) (progn 
                               (multiple-value-setq (value key-exist) (gethash item ht))

                               (if key-exist 
                                 (setf (gethash item ht) (+ value 1))
                                 (setf (gethash item ht) 1))))
            lst)

    ;; Extract only number of occurences.
    (maphash #'(lambda (key value) (push value count-lst)) ht)


    (setf tmp-sum (apply #'+ count-lst))

    ;;; Compute entropy.
    (- (apply #'+
      (mapcar #'(lambda (item) (progn 
                                 (setf tmp-div (/ item tmp-sum))
                                 (* tmp-div (log tmp-div 2)))) 
              count-lst)))
  ))

;;; Compute information gain for two children and one parent.
;;; Created for decision trees algorithm.
;;; TODO unit tests!
(defun information-gain (parent-entropy lst1 lst2)
  (let* ((lst1-len (length lst1))
         (lst2-len (length lst2))
         (total-len (+ lst1-len lst2-len))
         (lst1-entr (entropy lst1))
         (lst2-entr (entropy lst2)))

    (flet ((weighted-average (entr cnt tot)
                   (* entr (/ cnt tot))))

      (- parent-entropy (+
                          (weighted-average lst1-entr lst1-len total-len)
                          (weighted-average lst2-entr lst2-len total-len)))
  )))

;;; TODO unit tests!
(defun concatenate-with-space (str-lst &optional (complete ""))
  (let* ((space " ")
         (item-tmp (car str-lst))
         (item (if (stringp item-tmp)
                 item-tmp
                 (write-to-string item-tmp))))

    (if str-lst
      (concatenate-with-space
        (cdr str-lst)
        (concatenate 'string complete item space))
      complete)))

;;; TODO unit tests!
(defun load-dataset (dataset-path label-col-idx)
  (let* (;;; Load data.
        (dataset (matrix-from-data (read-csv dataset-path)))

        ;;; Data preprocessing.
        (data
          (remove-col label-col-idx dataset))
        (labels
          (matrix-from-data (nth-col label-col-idx dataset))))

  (values data labels)))

;;; Generate hash of parameters that are passed to training functions.
;;; Definition of parameters is expected as list of couples where
;;; the first is name of parameter and second is value.
;;; Example: '((num-epoch 10) (lr 0.001))
;;; TODO unit tests
(defun generate-params (param-lst)
  (let ((params (make-hash-table)))

    (mapcar #'(lambda (name-val)
                (setf (gethash (nth 0 name-val) params) (nth 1 name-val)))
            param-lst)

  params))

;;; Read item from hash table of parameters. If sought key does not exist return
;;; default value for particular parameter.
;;; TODO unit tests
(defun read-param (ht-params param-name default-val)
  (if (not ht-params)
    default-val
    (progn
      (multiple-value-setq (val found) (gethash param-name ht-params))
      (if (not found)
        default-val
        val))))

;;; TODO unit test
(defun aggregate-count (lst)
  (let ((value-count-ht (make-hash-table :test 'equal)))

    (mapcar #'(lambda (key)
                (progn
                  (multiple-value-setq (val found) (gethash key value-count-ht))
                   (if found
                     (setf (gethash key value-count-ht) (+ 1 val))
                     (setf (gethash key value-count-ht) 1))))

                lst)

  value-count-ht))

;;; TODO unit test
(defun aggregate-maximum (lst)
  (let ((value-count-ht (make-hash-table :test 'equal))
        (max-count 0)
        (max-key NIL))

  (flet ((update-maximum-value (key cnt)
            (progn
              (setf (gethash key value-count-ht) cnt)

              (if (> cnt max-count)
                (progn
                  (setf max-count cnt)
                  (setf max-key key))))))

    (mapcar #'(lambda (key)
                (progn
                  (multiple-value-setq (val found) (gethash key value-count-ht))

                   (if found
                     (update-maximum-value key (+ 1 val))
                     (update-maximum-value key 1))))

                lst)

  max-key)))
