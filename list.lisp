;;;; Martin Kersner, m.kersner@gmail.com
;;;; 2016/06/04

(in-package :lispml)

;;; TODO fix unit tests!
(defun range (start end &optional (step 1) (lst NIL))
  (if (< start end)
    (range (+ start step) end step (append lst (list start)))
    lst))

;;; http://aima.cs.berkeley.edu/lisp/utilities/utilities.lisp
;;; Return a list of n consecutive integers, by default starting at 0.
(defun iota (n &optional (start-at 0))
  (if (<= n 0) nil (cons start-at (iota (- n 1) (+ start-at 1)))))

;;; Return the last element of given list.
(defun last-elem (lst)
  (car (last lst)))

;;; Creates valid index in list.
;;; TODO unit test
(defun circular-index (idx lst)
  (let* ((l (length lst))
         (new-idx (mod (abs idx) l))
         (start-idx (if (< idx 0)
                      (if (equal new-idx 0)
                        (progn
                          (setf new-idx 0)
                          0)
                        (progn
                          (setf new-idx (- new-idx))
                          l))
                      0)))

    (+ start-idx new-idx)))

;;; Access list with positive or negative index.
(defun nth-pos-neg (idx lst)
  (nth (circular-index idx lst) lst))

;;; SETF expander for NTH set with positive or negative indiex.
(defun (setf nth-pos-neg) (val idx lst)
  (setf (nth (circular-index idx lst) lst) val))

;;; Find minimum value in a list.
(defun minimum (lst)
  (apply 'min lst))

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

;;; Find maximum value in a list.
(defun maximum-val (lst)
  (apply 'max lst))

;;; Find the index of maximum value in a list.
(defun maximum-idx (lst)
  (cadr (multiple-value-list (maximum lst))))

;;; Samuel Edwin Ward
;;; http://stackoverflow.com/questions/9444885/common-lisp-how-to-return-a-list-without-the-nth-element-of-a-given-list
;;; Remove the nth element from list.
(defun remove-nth (n list)
  (declare
    (type (integer 0) n)
    (type list list))
  (if (or (zerop n) (null list))
    (cdr list)
    (cons (car list) (remove-nth (1- n) (cdr list)))))

;;; Reorganize values from given list in random fashion.
(defun randomize-list (lst)
  (let* ((len-lst (length lst))
         (rand-idx 0) ; 0 or NIL?
         (rand-lst NIL))

    (dotimes (i len-lst)
      (setf rand-idx (random (- len-lst i)))
      (push (nth rand-idx lst) rand-lst)
      (setf lst (remove-nth rand-idx lst)))

    rand-lst))
