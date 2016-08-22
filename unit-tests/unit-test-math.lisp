;;;; Martin Kersner, m.kersner@gmail.com
;;;; 2016/06/03

(in-package :lispml)

(deftest test-basics ()
  (check
    ;; mean
    (= (mean '(1))     1)
    (= (mean '(1 2))   1.5)
    (= (mean '(1 2 3)) 2)

    ;; variance
    (= (var '(1))       0)
    (= (var '(1 2))     0.25)
    (= (var '(1 2 3 4)) 1.25)
  ))

(deftest test-math ()
  (combine-results
    (test-basics)
  ))
