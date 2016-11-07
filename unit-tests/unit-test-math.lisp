;;;; Martin Kersner, m.kersner@gmail.com
;;;; 2016/06/03

(in-package :lispml)

(deftest test-basics ()
  (check
    ;; even
    (eq (is-even 1)  NIL)
    (eq (is-even 2)  T)
    (eq (is-even 13) NIL)
    (eq (is-even 14) T)

    ;; odd 
    (eq (is-odd 3)  T)
    (eq (is-odd 4)  NIL)
    (eq (is-odd 15) T)
    (eq (is-odd 16) NIL)

    ;; mean
    (= (mean '(1))     1)
    (= (mean '(1 2))   1.5)
    (= (mean '(1 2 3)) 2)

    ;; variance
    (= (var '(1))       0)
    (= (var '(1 2))     0.25)
    (= (var '(1 2 3 4)) 1.25)

    ;; covariance
    (= (cov '(1 2) '(1 2))         0.5)
    (= (cov '(1 2 3 4) '(1 2 3 4)) 5/3)
  ))

(deftest test-math ()
  (combine-results
    (test-basics)
  ))
