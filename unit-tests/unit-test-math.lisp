;;;; Martin Kersner, m.kersner@gmail.com
;;;; 2016/06/03

(in-package :lispml)

(setf *math-namespace-unit-test* '())

(deftest test-basics ()
  (check
    ;; even
    (push 'is-even *math-namespace-unit-test*)
    (eq (is-even 1)  NIL)
    (eq (is-even 2)  T)
    (eq (is-even 13) NIL)
    (eq (is-even 14) T)

    ;; odd 
    (push 'is-odd *math-namespace-unit-test*)
    (eq (is-odd 3)  T)
    (eq (is-odd 4)  NIL)
    (eq (is-odd 15) T)
    (eq (is-odd 16) NIL)

    ;; mean
    (push 'mean *math-namespace-unit-test*)
    (= (mean '(1))     1)
    (= (mean '(1 2))   1.5)
    (= (mean '(1 2 3)) 2)

    ;; variance
    (push 'var *math-namespace-unit-test*)
    (= (var '(1))       0)
    (= (var '(1 2))     0.25)
    (= (var '(1 2 3 4)) 1.25)

    ;; covariance
    (push 'cov *math-namespace-unit-test*)
    (= (cov '(1 2) '(1 2))         0.5)
    (= (cov '(1 2 3 4) '(1 2 3 4)) 5/3)
  ))

(deftest test-math ()
  (combine-results
    (test-basics)

    (unit-test-coverage *math-namespace* *math-namespace-unit-test* "math")))
