;;;; Martin Kersner, m.kersner@gmail.com
;;;; 2016/04/27 

;;; TODO check error conditions in tests

(in-package :lispml)

(setf *matrix-namespace-unit-test* '())

(deftest test-generate-matrix ()
  (check
    ;; empty-matrix
    (push 'empty-matrix *matrix-namespace-unit-test*)
    (compare-matrix (empty-matrix 1 1) (make-matrix :rows 1 :cols 1 :data '((nil))))
    (compare-matrix (empty-matrix 2 1) (make-matrix :rows 2 :cols 1 :data '((nil)(nil))))
    (compare-matrix (empty-matrix 1 2) (make-matrix :rows 1 :cols 2 :data '((nil nil))))
    (compare-matrix (empty-matrix 2 2) (make-matrix :rows 2 :cols 2 :data '((nil nil)(nil nil))))
    (compare-matrix (empty-matrix 2 3) (make-matrix :rows 2 :cols 3 :data '((nil nil nil)(nil nil nil))))

    ;; initialize-matrix
    (push 'initialize-matrix *matrix-namespace-unit-test*)
    (compare-matrix (initialize-matrix 1 1 1) (make-matrix :rows 1 :cols 1 :data '((1))))
    (compare-matrix (initialize-matrix 2 1 2) (make-matrix :rows 2 :cols 1 :data '((2)(2))))
    (compare-matrix (initialize-matrix 1 2 3) (make-matrix :rows 1 :cols 2 :data '((3 3))))
    (compare-matrix (initialize-matrix 2 2 4) (make-matrix :rows 2 :cols 2 :data '((4 4)(4 4))))
    (compare-matrix (initialize-matrix 2 3 5) (make-matrix :rows 2 :cols 3 :data '((5 5 5)(5 5 5))))

    ;; matrix-from-data
    (push 'matrix-from-data *matrix-namespace-unit-test*)
    (compare-matrix (matrix-from-data '((1))) (make-matrix :rows 1 :cols 1 :data '((1))))
    (compare-matrix (matrix-from-data '((1 2))) (make-matrix :rows 1 :cols 2 :data '((1 2))))
    (compare-matrix (matrix-from-data '((1)(2))) (make-matrix :rows 2 :cols 1 :data '((1)(2))))
    (compare-matrix (matrix-from-data '((1 2)(3 4))) (make-matrix :rows 2 :cols 2 :data '((1 2)(3 4))))
    (compare-matrix (matrix-from-data '((1 2 3)(4 5 6)(7 8 9))) (make-matrix :rows 3 :cols 3 :data '((1 2 3)(4 5 6)(7 8 9))))

    ;; empty-matrix-like
    (push 'empty-matrix-like *matrix-namespace-unit-test*)
    (compare-matrix (empty-matrix-like (matrix-from-data '((0))))        (matrix-from-data '((NIL))))
    (compare-matrix (empty-matrix-like (matrix-from-data '((0)(1))))     (matrix-from-data '((NIL)(NIL))))
    (compare-matrix (empty-matrix-like (matrix-from-data '((0 1))))      (matrix-from-data '((NIL NIL))))
    (compare-matrix (empty-matrix-like (matrix-from-data '((0 1)(2 3)))) (matrix-from-data '((NIL NIL)(NIL NIL))))

    ;; zero-matrix-like
    (push 'zero-matrix-like *matrix-namespace-unit-test*)
    (compare-matrix (zero-matrix-like (matrix-from-data '((0))))        (matrix-from-data '((0))))
    (compare-matrix (zero-matrix-like (matrix-from-data '((0)(1))))     (matrix-from-data '((0)(0))))
    (compare-matrix (zero-matrix-like (matrix-from-data '((0 1))))      (matrix-from-data '((0 0))))
    (compare-matrix (zero-matrix-like (matrix-from-data '((0 1)(2 3)))) (matrix-from-data '((0 0)(0 0))))
  ))

(deftest test-transpose-matrix ()
  (check
    ;; transpose
    (push 'transpose *matrix-namespace-unit-test*)
    (compare-matrix (transpose (make-matrix :rows 1 :cols 1 :data '((1))))
                    (make-matrix :rows 1 :cols 1 :data '((1))))
    (compare-matrix (transpose (make-matrix :rows 1 :cols 2 :data '((1 2))))
                    (make-matrix :rows 2 :cols 1 :data '((1)(2))))
    (compare-matrix (transpose (make-matrix :rows 2 :cols 1 :data '((1)(2))))
                    (make-matrix :rows 1 :cols 2 :data '((1 2))))
    (compare-matrix (transpose (make-matrix :rows 2 :cols 2 :data '((1 2)(3 4))))
                    (make-matrix :rows 2 :cols 2 :data '((1 3)(2 4))))
    (compare-matrix (transpose (make-matrix :rows 2 :cols 3 :data '((1 2 3)(4 5 6))))
                    (make-matrix :rows 3 :cols 2 :data '((1 4)(2 5)(3 6))))
    (compare-matrix (transpose (make-matrix :rows 3 :cols 2 :data '((1 2)(3 4)(5 6))))
                    (make-matrix :rows 2 :cols 3 :data '((1 3 5)(2 4 6))))
    (compare-matrix (transpose (make-matrix :rows 3 :cols 3 :data '((1 2 3)(4 5 6)(7 8 9))))
                    (make-matrix :rows 3 :cols 3 :data '((1 4 7)(2 5 8)(3 6 9))))
  ))

(deftest test-access-matrix ()
  (check
    ;; nth-row
    (push 'nth-row *matrix-namespace-unit-test*)
    (equal (nth-row 0 (matrix-from-data '((1)))) '((1)))
    (equal (nth-row 0 (matrix-from-data '((1 2)))) '((1 2)))
    (equal (nth-row 0 (matrix-from-data '((1)(2)))) '((1)))
    (equal (nth-row 0 (matrix-from-data '((1 2)(3 4)))) '((1 2)))
    (equal (nth-row 1 (matrix-from-data '((1 2)(3 4)))) '((3 4)))
    (equal (nth-row 0 (matrix-from-data '((1 2)(3 4)(5 6)))) '((1 2)))
    (equal (nth-row 1 (matrix-from-data '((1 2)(3 4)(5 6)))) '((3 4)))
    (equal (nth-row 2 (matrix-from-data '((1 2)(3 4)(5 6)))) '((5 6)))

    ;; nth-col
    (push 'nth-col *matrix-namespace-unit-test*)
    (equal (nth-col 0 (matrix-from-data '((1)))) '((1)))
    (equal (nth-col 0 (matrix-from-data '((1 2)))) '((1)))
    (equal (nth-col 1 (matrix-from-data '((1 2)))) '((2)))
    (equal (nth-col 0 (matrix-from-data '((1)(2)))) '((1)(2)))
    (equal (nth-col 0 (matrix-from-data '((1 2)(3 4)))) '((1)(3)))
    (equal (nth-col 1 (matrix-from-data '((1 2)(3 4)))) '((2)(4)))
    (equal (nth-col 0 (matrix-from-data '((1 2)(3 4)(5 6)))) '((1)(3)(5)))
    (equal (nth-col 1 (matrix-from-data '((1 2)(3 4)(5 6)))) '((2)(4)(6)))

    ;; []
    (push '[] *matrix-namespace-unit-test*)
    (compare-matrix ([] 0 0 (matrix-from-data '((1)(2)(3)))) (matrix-from-data '((1))))
    (compare-matrix ([] 0 1 (matrix-from-data '((1)(2)(3)))) (matrix-from-data '((1)(2))))
    (compare-matrix ([] 0 2 (matrix-from-data '((1)(2)(3)))) (matrix-from-data '((1)(2)(3))))
    (compare-matrix ([] 1 2 (matrix-from-data '((1)(2)(3)))) (matrix-from-data '((2)(3))))
    (compare-matrix ([] 0 0 (matrix-from-data '((1 1)(2 2)(3 3)))) (matrix-from-data '((1 1))))
    (compare-matrix ([] 0 1 (matrix-from-data '((1 1)(2 2)(3 3)))) (matrix-from-data '((1 1)(2 2))))
    (compare-matrix ([] 0 2 (matrix-from-data '((1 1)(2 2)(3 3)))) (matrix-from-data '((1 1)(2 2)(3 3))))
    (compare-matrix ([] 1 2 (matrix-from-data '((1 1)(2 2)(3 3)))) (matrix-from-data '((2 2)(3 3))))

    ;; [][]
    (push '[][] *matrix-namespace-unit-test*)
    (equal ([][] 0 0 (matrix-from-data '((1))))   1)
    (equal ([][] 0 1 (matrix-from-data '((1 2)))) 2)
    (equal ([][] 0 0 (matrix-from-data '((1 2)(3 4)))) 1)
    (equal ([][] 0 1 (matrix-from-data '((1 2)(3 4)))) 2)
    (equal ([][] 1 0 (matrix-from-data '((1 2)(3 4)))) 3)
    (equal ([][] 1 1 (matrix-from-data '((1 2)(3 4)))) 4)
  ))

(deftest test-matrix-modifications()
  (check
    ;; remove-col
    (push 'remove-col *matrix-namespace-unit-test*)
    (compare-matrix (remove-col 0 (matrix-from-data '((1)))) (matrix-from-data '(()))) ; correct?
    (compare-matrix (remove-col 0 (matrix-from-data '((1 2)))) (matrix-from-data '((2))))
    (compare-matrix (remove-col 1 (matrix-from-data '((1 2 3)))) (matrix-from-data '((1 3))))
    (compare-matrix (remove-col 0 (matrix-from-data '((1 2 3)(4 5 6)))) (matrix-from-data '((2 3)(5 6))))
    (compare-matrix (remove-col 1 (matrix-from-data '((1 2 3)(4 5 6)))) (matrix-from-data '((1 3)(4 6))))
    (compare-matrix (remove-col 2 (matrix-from-data '((1 2 3)(4 5 6)))) (matrix-from-data '((1 2)(4 5))))

    ;; remove-row
    (push 'remove-row *matrix-namespace-unit-test*)
    ;(compare-matrix (remove-row 0 (matrix-from-data '((1)))) (matrix-from-data '(()))) ; TODO
    (compare-matrix (remove-row 0 (matrix-from-data '((1)(2)(3)))) (matrix-from-data '((2)(3))))
    (compare-matrix (remove-row 1 (matrix-from-data '((1)(2)(3)))) (matrix-from-data '((1)(3))))
    (compare-matrix (remove-row 1 (matrix-from-data '((1)(2)(3)))) (matrix-from-data '((1)(3))))

    ;; prefix-const-val
    (push 'prefix-const-val *matrix-namespace-unit-test*)
    (compare-matrix (prefix-const-val 0 (matrix-from-data '((1)))) (matrix-from-data '((0 1))))
    (compare-matrix (prefix-const-val 0 (matrix-from-data '((1 2)))) (matrix-from-data '((0 1 2))))
    (compare-matrix (prefix-const-val 0 (matrix-from-data '((1)(2)))) (matrix-from-data '((0 1)(0 2))))
    (compare-matrix (prefix-const-val 0 (matrix-from-data '((1 2)(3 4)))) (matrix-from-data '((0 1 2)(0 3 4))))

    ;; suffix-const-val
    (push 'suffix-const-val *matrix-namespace-unit-test*)
    (compare-matrix (suffix-const-val 0 (matrix-from-data '((1)))) (matrix-from-data '((1 0))))
    (compare-matrix (suffix-const-val 0 (matrix-from-data '((1 2)))) (matrix-from-data '((1 2 0))))
    (compare-matrix (suffix-const-val 0 (matrix-from-data '((1)(2)))) (matrix-from-data '((1 0)(2 0))))
    (compare-matrix (suffix-const-val 0 (matrix-from-data '((1 2)(3 4)))) (matrix-from-data '((1 2 0)(3 4 0))))

    ;; insert-const-val
    (push 'insert-const-val *matrix-namespace-unit-test*)
    (compare-matrix (insert-const-val 0 0 (matrix-from-data '((1)))) (matrix-from-data '((0 1))))
    (compare-matrix (insert-const-val 1 0 (matrix-from-data '((1)))) (matrix-from-data '((1 0))))
    (compare-matrix (insert-const-val 0 0 (matrix-from-data '((1 2)))) (matrix-from-data '((0 1 2))))
    (compare-matrix (insert-const-val 1 0 (matrix-from-data '((1 2)))) (matrix-from-data '((1 0 2))))
    (compare-matrix (insert-const-val 2 0 (matrix-from-data '((1 2)))) (matrix-from-data '((1 2 0))))
    (compare-matrix (insert-const-val 0 0 (matrix-from-data '((1)(2)))) (matrix-from-data '((0 1)(0 2))))
    (compare-matrix (insert-const-val 1 0 (matrix-from-data '((1)(2)))) (matrix-from-data '((1 0)(2 0))))
    (compare-matrix (insert-const-val 0 0 (matrix-from-data '((1 2)(3 4)))) (matrix-from-data '((0 1 2)(0 3 4))))
    (compare-matrix (insert-const-val 1 0 (matrix-from-data '((1 2)(3 4)))) (matrix-from-data '((1 0 2)(3 0 4))))
    (compare-matrix (insert-const-val 2 0 (matrix-from-data '((1 2)(3 4)))) (matrix-from-data '((1 2 0)(3 4 0))))
  ))

(deftest test-dot-product ()
  (check
    ;; dot
    (push 'dot *matrix-namespace-unit-test*)
    (compare-matrix (dot (matrix-from-data '((1))) (matrix-from-data '((1)))) (matrix-from-data '((1))))
    (compare-matrix (dot (matrix-from-data '((1))) (matrix-from-data '((1 2)))) (matrix-from-data '((1 2))))
    (compare-matrix (dot (matrix-from-data '((1 2))) (matrix-from-data '((1)(2)))) (matrix-from-data '((5))))
    (compare-matrix (dot (matrix-from-data '((1 2)(3 4))) (matrix-from-data '((1 2)(3 4)))) (matrix-from-data '((7 10)(15 22))))
    (compare-matrix (dot (matrix-from-data '((1 2 3)(4 5 6))) (matrix-from-data '((1 2)(3 4)(5 6)))) (matrix-from-data '((22 28)(49 64))))
  ))

(deftest test-element-wise-operations ()
  (check
    ;; add
    (push 'add *matrix-namespace-unit-test*)
    (compare-matrix (add (matrix-from-data '((1))) (matrix-from-data '((1)))) (matrix-from-data '((2))))
    (compare-matrix (add (matrix-from-data '((1 2))) (matrix-from-data '((3 4)))) (matrix-from-data '((4 6))))
    (compare-matrix (add (matrix-from-data '((1)(2))) (matrix-from-data '((3)(4)))) (matrix-from-data '((4)(6))))
    (compare-matrix (add (matrix-from-data '((1 2)(3 4))) (matrix-from-data '((5 6)(7 8)))) (matrix-from-data '((6 8)(10 12))))
    
    ;; subtract
    (push 'subtract *matrix-namespace-unit-test*)
    (compare-matrix (subtract (matrix-from-data '((1))) (matrix-from-data '((1)))) (matrix-from-data '((0))))
    (compare-matrix (subtract (matrix-from-data '((1 2))) (matrix-from-data '((2 4)))) (matrix-from-data '((-1 -2))))
    (compare-matrix (subtract (matrix-from-data '((1)(2))) (matrix-from-data '((3)(5)))) (matrix-from-data '((-2)(-3))))
    (compare-matrix (subtract (matrix-from-data '((1 2)(3 4))) (matrix-from-data '((4 7)(5 10)))) (matrix-from-data '((-3 -5)(-2 -6))))
    
    ;; add-value
    (push 'add-value *matrix-namespace-unit-test*)
    (compare-matrix (add-value 1 (matrix-from-data '((1)))) (matrix-from-data '((2))))
    (compare-matrix (add-value 2 (matrix-from-data '((1 2)))) (matrix-from-data '((3 4))))
    (compare-matrix (add-value 3 (matrix-from-data '((1 2)(3 4)))) (matrix-from-data '((4 5)(6 7))))
    
    ;; multiply (by constant)
    (push 'multiply *matrix-namespace-unit-test*)
    (compare-matrix (multiply 2 (matrix-from-data '((1)))) (matrix-from-data '((2))))
    (compare-matrix (multiply 3 (matrix-from-data '((1 2)))) (matrix-from-data '((3 6))))
    (compare-matrix (multiply 3 (matrix-from-data '((1)(2)))) (matrix-from-data '((3)(6))))
    (compare-matrix (multiply 4 (matrix-from-data '((1 2)(3 4)))) (matrix-from-data '((4 8)(12 16))))
    
    ;; value-matrix-subtract
    (push 'value-matrix-subtract *matrix-namespace-unit-test*)
    (compare-matrix (value-matrix-subtract 0 (matrix-from-data '((1)))) (matrix-from-data '((-1))))
    (compare-matrix (value-matrix-subtract 1 (matrix-from-data '((1)(2)))) (matrix-from-data '((0)(-1))))
    (compare-matrix (value-matrix-subtract 2 (matrix-from-data '((1 2)(3 4)))) (matrix-from-data '((1 0)(-1 -2))))
    
    ;; matrix-mult
    (push 'matrix-mult *matrix-namespace-unit-test*)
    (compare-matrix (matrix-mult  (matrix-from-data '((1))) (matrix-from-data '((-1)))) (matrix-from-data '((-1))))
    (compare-matrix (matrix-mult  (matrix-from-data '((1)(2))) (matrix-from-data '((2)(8)))) (matrix-from-data '((2)(16))))
    (compare-matrix (matrix-mult  (matrix-from-data '((1 2)(3 4))) (matrix-from-data '((2 3)(9 0)))) (matrix-from-data '((2 6)(27 0))))
    (compare-matrix (matrix-mult  (matrix-from-data '((1 2 3)(4 5 6)(7 8 9))) (matrix-from-data '((0 0 0)(7 6 5)(1 2 3)))) (matrix-from-data '((0 0 0)(28 30 30)(7 16 27))))
    
    ;; matrix-div
    (push 'matrix-div *matrix-namespace-unit-test*)
    (compare-matrix (matrix-div (matrix-from-data '((1))) (matrix-from-data '((-1)))) (matrix-from-data '((-1))))
    (compare-matrix (matrix-div (matrix-from-data '((2)(8))) (matrix-from-data '((2)(4)))) (matrix-from-data '((1)(2))))
    (compare-matrix (matrix-div (matrix-from-data '((1 4)(6 10))) (matrix-from-data '((1 2)(3 5)))) (matrix-from-data '((1 2)(2 2))))
    (compare-matrix (matrix-div (matrix-from-data '((10 9 8)(7 6 5)(4 3 2))) (matrix-from-data '((5 3 4)(7 3 5)(1 3 2)))) (matrix-from-data '((2 3 2)(1 2 1)(4 1 1))))
    
    ;; subtract-row
    (push 'subtract-row *matrix-namespace-unit-test*)
    (compare-matrix (subtract-row (matrix-from-data '((1))) (matrix-from-data '((-1)))) (matrix-from-data '((2))))
    (compare-matrix (subtract-row (matrix-from-data '((1)(2))) (matrix-from-data '((1)))) (matrix-from-data '((0)(1))))
    (compare-matrix (subtract-row (matrix-from-data '((1 2)(3 4))) (matrix-from-data '((2 3)))) (matrix-from-data '((-1 -1)(1 1))))
    (compare-matrix (subtract-row (matrix-from-data '((1 2 3)(4 5 6)(7 8 9))) (matrix-from-data '((1 5 9)))) (matrix-from-data '((0 -3 -6)(3 0 -3)(6 3 0))))
    
    ;; subtract-col
    (push 'subtract-col *matrix-namespace-unit-test*)
    (compare-matrix (subtract-col (matrix-from-data '((1))) (matrix-from-data '((-1)))) (matrix-from-data '((2))))
    (compare-matrix (subtract-col (matrix-from-data '((1)(2))) (matrix-from-data '((-1)(-1)))) (matrix-from-data '((2)(3))))
    (compare-matrix (subtract-col (matrix-from-data '((1 2)(2 3))) (matrix-from-data '((1)(2)))) (matrix-from-data '((0 1)(0 1))))
    (compare-matrix (subtract-col (matrix-from-data '((1 2 3)(2 3 4)(3 4 5))) (matrix-from-data '((1)(2)(3)))) (matrix-from-data '((0 1 2)(0 1 2)(0 1 2))))

    ;; subtract-val-col
    (push 'subtract-val-col *matrix-namespace-unit-test*)
    (compare-matrix (subtract-val-col 1 0 (matrix-from-data '((1 2 3)))) (matrix-from-data '((0 2 3))))
    (compare-matrix (subtract-val-col 1 1 (matrix-from-data '((1 2 3)))) (matrix-from-data '((1 1 3))))
    (compare-matrix (subtract-val-col 1 2 (matrix-from-data '((1 2 3)))) (matrix-from-data '((1 2 2))))
    (compare-matrix (subtract-val-col 1 0 (matrix-from-data '((1 2 3)(4 5 6)))) (matrix-from-data '((0 2 3)(3 5 6))))
    (compare-matrix (subtract-val-col 1 1 (matrix-from-data '((1 2 3)(4 5 6)))) (matrix-from-data '((1 1 3)(4 4 6))))
    (compare-matrix (subtract-val-col 1 2 (matrix-from-data '((1 2 3)(4 5 6)))) (matrix-from-data '((1 2 2)(4 5 5))))

    ;; power
    (push 'power *matrix-namespace-unit-test*)
    (compare-matrix (power 3 (matrix-from-data '((2)))) (matrix-from-data '((8))))
    (compare-matrix (power 2 (matrix-from-data '((1 2)))) (matrix-from-data '((1 4))))
    (compare-matrix (power 2 (matrix-from-data '((1 2)(3 4)))) (matrix-from-data '((1 4)(9 16))))
    (compare-matrix (power 2 (matrix-from-data '((1 2 3)(4 5 6)(7 8 9)))) (matrix-from-data '((1 4 9)(16 25 36)(49 64 81))))
  ))

(deftest test-aggregating-functions ()
  (check
    ;; sum-rows
    (push 'sum-rows *matrix-namespace-unit-test*)
    (compare-matrix (sum-rows (matrix-from-data '((1)))) (matrix-from-data '((1))))
    (compare-matrix (sum-rows (matrix-from-data '((1 2)))) (matrix-from-data '((3))))
    (compare-matrix (sum-rows (matrix-from-data '((1 2 3)))) (matrix-from-data '((6))))
    (compare-matrix (sum-rows (matrix-from-data '((1 2 3)(4 5 6)))) (matrix-from-data '((6)(15))))
    
    ;; sum-cols
    (push 'sum-cols *matrix-namespace-unit-test*)
    (compare-matrix (sum-cols (matrix-from-data '((1)))) (matrix-from-data '((1))))
    (compare-matrix (sum-cols (matrix-from-data '((1 2 3)))) (matrix-from-data '((1 2 3))))
    (compare-matrix (sum-cols (matrix-from-data '((1 2 3)(1 2 3)))) (matrix-from-data '((2 4 6))))
    (compare-matrix (sum-cols (matrix-from-data '((1 2 3)(1 2 3)(1 2 3)))) (matrix-from-data '((3 6 9))))
    
    ;; sum
    (push 'sum *matrix-namespace-unit-test*)
    (equal (sum (matrix-from-data '((1))))         1)
    (equal (sum (matrix-from-data '((1)(2))))      3)
    (equal (sum (matrix-from-data '((1)(2)(3))))   6)
    (equal (sum (matrix-from-data '((1 2))))       3)
    (equal (sum (matrix-from-data '((1 2 3))))     6)
    (equal (sum (matrix-from-data '((1 2)(3 4)))) 10)
    
    ;; mean-cols
    (push 'mean-cols *matrix-namespace-unit-test*)
    (compare-matrix (mean-cols (matrix-from-data '((1 2 3)))) (matrix-from-data '((1 2 3))))
    (compare-matrix (mean-cols (matrix-from-data '((1 2 3)(3 4 5)))) (matrix-from-data '((2 3 4))))
    (compare-matrix (mean-cols (matrix-from-data '((1 2 3)(3 4 5)(5 6 7)))) (matrix-from-data '((3 4 5))))

    ;; mean-rows
    (push 'mean-rows *matrix-namespace-unit-test*)
    (compare-matrix (mean-rows (matrix-from-data '((1)))) (matrix-from-data '((1))))
    (compare-matrix (mean-rows (matrix-from-data '((1)(2)))) (matrix-from-data '((1)(2))))
    (compare-matrix (mean-rows (matrix-from-data '((1 2 3)))) (matrix-from-data '((2))))
    (compare-matrix (mean-rows (matrix-from-data '((1 2 3)(2 3 4)))) (matrix-from-data '((2)(3))))
  ))

(deftest test-sort-functions ()
  (check
    ;; arg-sort-col-mat
    (push 'arg-sort-col-mat *matrix-namespace-unit-test*)
    (equal (arg-sort-col-mat (matrix-from-data '((1)))) '(0))
    (equal (arg-sort-col-mat (matrix-from-data '((1)(0)))) '(1 0))
    (equal (arg-sort-col-mat (matrix-from-data '((5)(4)(3)(2)(1)(0)))) '(5 4 3 2 1 0))
   ))

(deftest test-mathematical-functions ()
  (check
   ;; sigmoid-base
    (push 'sigmoid-base *matrix-namespace-unit-test*)
   (equal (/ 1 2) (sigmoid-base 0))
   (< (abs (- (sigmoid-base 10) 1)) 1E-4)
   (< (abs (- (sigmoid-base -10) 0)) 1E-4)
  ))

(deftest test-value-extremes ()
  (check
    ;; nth-col-max
    (push 'nth-col-max *matrix-namespace-unit-test*)
    (equal (nth-col-max 0 (matrix-from-data '((1 2 3)))) 1)
    (equal (nth-col-max 1 (matrix-from-data '((1 2 3)))) 2)
    (equal (nth-col-max 2 (matrix-from-data '((1 2 3)))) 3)
    (equal (nth-col-max 0 (matrix-from-data '((1 2 3)(4 5 6)))) 4)
    (equal (nth-col-max 1 (matrix-from-data '((1 2 3)(4 5 6)))) 5)
    (equal (nth-col-max 2 (matrix-from-data '((2 2 3)(4 5 6)))) 6)
    (equal (nth-col-max 0 (matrix-from-data '((1 2 3)(7 8 9)(4 5 6)))) 7)
    (equal (nth-col-max 1 (matrix-from-data '((1 2 3)(7 8 9)(4 5 6)))) 8)
    (equal (nth-col-max 2 (matrix-from-data '((2 2 3)(7 8 9)(4 5 6)))) 9)

    ;; nth-col-min
    (push 'nth-col-min *matrix-namespace-unit-test*)
    (equal (nth-col-min 0 (matrix-from-data '((1 2 3)))) 1)
    (equal (nth-col-min 1 (matrix-from-data '((1 2 3)))) 2)
    (equal (nth-col-min 2 (matrix-from-data '((1 2 3)))) 3)
    (equal (nth-col-min 0 (matrix-from-data '((1 2 3)(4 5 6)))) 1)
    (equal (nth-col-min 1 (matrix-from-data '((1 2 3)(4 5 6)))) 2)
    (equal (nth-col-min 2 (matrix-from-data '((2 2 3)(4 5 6)))) 3)
    (equal (nth-col-min 0 (matrix-from-data '((7 8 9)(1 2 3)(4 5 6)))) 1)
    (equal (nth-col-min 1 (matrix-from-data '((7 8 9)(1 2 3)(4 5 6)))) 2)
    (equal (nth-col-min 2 (matrix-from-data '((7 8 9)(2 2 3)(4 5 6)))) 3)

    ;; nth-row-max
    (push 'nth-row-max *matrix-namespace-unit-test*)
    (equal (nth-row-max 0 (matrix-from-data '((1)(2)(3)))) 1)
    (equal (nth-row-max 1 (matrix-from-data '((1)(2)(3)))) 2)
    (equal (nth-row-max 2 (matrix-from-data '((1)(2)(3)))) 3)
    (equal (nth-row-max 0 (matrix-from-data '((1 2 3)))) 3)
    (equal (nth-row-max 1 (matrix-from-data '((1 2 3)(4 5 6)))) 6)
    (equal (nth-row-max 0 (matrix-from-data '((1 2 3)(7 8 9)(4 5 6)))) 3)
    (equal (nth-row-max 1 (matrix-from-data '((1 2 3)(7 8 9)(4 5 6)))) 9)
    (equal (nth-row-max 2 (matrix-from-data '((2 2 3)(7 8 9)(4 5 6)))) 6)

    ;; nth-row-min
    (push 'nth-row-min *matrix-namespace-unit-test*)
    (equal (nth-row-min 0 (matrix-from-data '((1)(2)(3)))) 1)
    (equal (nth-row-min 1 (matrix-from-data '((1)(2)(3)))) 2)
    (equal (nth-row-min 2 (matrix-from-data '((1)(2)(3)))) 3)
    (equal (nth-row-min 0 (matrix-from-data '((1 2 3)))) 1)
    (equal (nth-row-min 1 (matrix-from-data '((1 2 3)(4 5 6)))) 4)
    (equal (nth-row-min 0 (matrix-from-data '((1 2 3)(7 8 9)(4 5 6)))) 1)
    (equal (nth-row-min 1 (matrix-from-data '((1 2 3)(7 8 9)(4 5 6)))) 7)
    (equal (nth-row-min 2 (matrix-from-data '((2 2 3)(7 8 9)(4 5 6)))) 4)
  ))

(deftest test-shuffling ()
  (check
    ;; shuffle-rows-spec
    (push 'shuffle-rows-spec *matrix-namespace-unit-test*)
    (compare-matrix (shuffle-rows-spec (matrix-from-data '((1)(2)(3))) '(0 1 2)) (matrix-from-data '((1)(2)(3))))
    (compare-matrix (shuffle-rows-spec (matrix-from-data '((1)(2)(3))) '(2 1 0)) (matrix-from-data '((3)(2)(1))))
    (compare-matrix (shuffle-rows-spec (matrix-from-data '((1)(2)(3))) '(1 2 0)) (matrix-from-data '((2)(3)(1))))
    (compare-matrix (shuffle-rows-spec (matrix-from-data '((4)(3)(2))) '(1 1 1)) (matrix-from-data '((3)(3)(3))))
    (compare-matrix (shuffle-rows-spec (matrix-from-data '((1 2)(2 3)(3 4))) '(2 1 0)) (matrix-from-data '((3 4)(2 3)(1 2))))

    ;; det
    (push 'det *matrix-namespace-unit-test*)
    (eq (det (matrix-from-data '((4 6)(3 8)))) 14)
    (eq (det (matrix-from-data '((1 1 1)(4 3 4)(1 0 1)))) 0)
    (eq (det (matrix-from-data '((6 1 1)(4 -2 5)(2 8 7)))) -306)
    (eq (det (matrix-from-data '((1 3 2)(4 1 3)(2 5 2)))) 17)

    ;; inverse of matrix
    (push 'inv *matrix-namespace-unit-test*)
    (compare-matrix (inv (matrix-from-data '((3 0 2)(2 0 -2)(0 1 1)))) (matrix-from-data '((1/5 1/5 0)(-1/5 3/10 1)(1/5 -3/10 0))))
    (compare-matrix (inv (matrix-from-data '((1 2)(3 4)))) (matrix-from-data '((-2 1)(3/2 -1/2))))

    ;; apply-matrix
    (push 'apply-matrix *matrix-namespace-unit-test*)
    (compare-matrix (apply-matrix (lambda (x) (+ x 1)) (matrix-from-data '((1)))) (matrix-from-data '((2))))
    (compare-matrix (apply-matrix (lambda (x) (+ (* x 3) 1)) (matrix-from-data '((1 2)(3 4)))) (matrix-from-data '((4 7)(10 13))))
    (compare-matrix (apply-matrix (lambda (x) (expt x 2)) (matrix-from-data '((2 2 2)(3 3 3)))) (matrix-from-data '((4 4 4)(9 9 9))))
  ))

(deftest test-matrix ()
  (combine-results
    (test-generate-matrix)
    (test-matrix-modifications)
    (test-transpose-matrix)
    (test-access-matrix)
    (test-element-wise-operations)
    (test-dot-product)
    (test-aggregating-functions)
    (test-sort-functions)
    (test-mathematical-functions)
    (test-value-extremes)
    (test-shuffling)

    (unit-test-coverage *matrix-namespace* *matrix-namespace-unit-test* "matrix")))

(test-matrix)
