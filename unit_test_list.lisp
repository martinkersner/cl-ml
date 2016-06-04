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

(deftest test-all ()
  (combine-results
    (test-range)
  ))
