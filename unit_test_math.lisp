;;;; Martin Kersner, m.kersner@gmail.com
;;;; 2016/06/03

(load "unit_test")
(load "math")

(deftest test-maximum ()
  (check
    ;; base maximum function, return both maximum value and index
    (equal (multiple-value-list (maximum '()))         '(NIL 0))
    (equal (multiple-value-list (maximum '(1)))        '(1 0))
    (equal (multiple-value-list (maximum '(9 8 7)))    '(9 0))
    (equal (multiple-value-list (maximum '(9 10 8)))   '(10 1))
    (equal (multiple-value-list (maximum '(9 10 11)))  '(11 2))
    (equal (multiple-value-list (maximum '(9 10 10)))  '(10 1))
    (equal (multiple-value-list (maximum '(10 10 10))) '(10 0))
  ))

(deftest test-all ()
  (combine-results
    (test-maximum)
   ))
