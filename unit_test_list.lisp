;;;; Martin Kersner, m.kersner@gmail.com
;;;; 2016/06/04

(load "unit_test")
(load "list")

(deftest test-range ()
  (check
    ;; create ascending series of numbers
    (equal (range 0 0) '(0))
    (equal (range 0 4) '(0 1 2 3 4))
    (equal (range 0 4 2) '(0 2 4))
    (equal (range 0 4 3) '(0 3 ))
  ))

(deftest test-nth-pos-neg ()
  (defparameter *list* '(1 2 3 4 5 6))

  (check
    ;; nth-pos-neg
    (equal (setf (nth-pos-neg 0 *list*) 99) 99)
    (equal *list* '(99 2 3 4 5 6))
    (equal (setf (nth-pos-neg 2 *list*) 88) 88)
    (equal *list* '(99 2 88 4 5 6))
    (equal (setf (nth-pos-neg -1 *list*) 77) 77)
    (equal *list* '(99 2 88 4 5 77))
    (equal (setf (nth-pos-neg -5 *list*) 66) 66)
    (equal *list* '(99 66 88 4 5 77))
  ))

(deftest test-all ()
  (combine-results
    (test-range)
    (test-nth-pos-neg)
  ))
