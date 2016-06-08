;;;; Martin Kersner, m.kersner@gmail.com
;;;; 2016/04/27 

;;; TODO check error conditions in tests

(load "unit_test")
(load "matrix")

(deftest test-generate-matrix ()
  (check
    ;; empty-matrix
    (compare-matrix (empty-matrix 1 1) (make-matrix :rows 1 :cols 1 :data '((nil))))
    (compare-matrix (empty-matrix 2 1) (make-matrix :rows 2 :cols 1 :data '((nil)(nil))))
    (compare-matrix (empty-matrix 1 2) (make-matrix :rows 1 :cols 2 :data '((nil nil))))
    (compare-matrix (empty-matrix 2 2) (make-matrix :rows 2 :cols 2 :data '((nil nil)(nil nil))))
    (compare-matrix (empty-matrix 2 3) (make-matrix :rows 2 :cols 3 :data '((nil nil nil)(nil nil nil))))

    ;; initialize-matrix
    (compare-matrix (initialize-matrix 1 1 1) (make-matrix :rows 1 :cols 1 :data '((1))))
    (compare-matrix (initialize-matrix 2 1 2) (make-matrix :rows 2 :cols 1 :data '((2)(2))))
    (compare-matrix (initialize-matrix 1 2 3) (make-matrix :rows 1 :cols 2 :data '((3 3))))
    (compare-matrix (initialize-matrix 2 2 4) (make-matrix :rows 2 :cols 2 :data '((4 4)(4 4))))
    (compare-matrix (initialize-matrix 2 3 5) (make-matrix :rows 2 :cols 3 :data '((5 5 5)(5 5 5))))

    ;; matrix-from-data
    (compare-matrix (matrix-from-data '((1))) (make-matrix :rows 1 :cols 1 :data '((1))))
    (compare-matrix (matrix-from-data '((1 2))) (make-matrix :rows 1 :cols 2 :data '((1 2))))
    (compare-matrix (matrix-from-data '((1)(2))) (make-matrix :rows 2 :cols 1 :data '((1)(2))))
    (compare-matrix (matrix-from-data '((1 2)(3 4))) (make-matrix :rows 2 :cols 2 :data '((1 2)(3 4))))
    (compare-matrix (matrix-from-data '((1 2 3)(4 5 6)(7 8 9))) (make-matrix :rows 3 :cols 3 :data '((1 2 3)(4 5 6)(7 8 9))))
  ))

(deftest test-transpose-matrix ()
  (check
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
    (equal (nth-row 0 (matrix-from-data '((1)))) '((1)))
    (equal (nth-row 0 (matrix-from-data '((1 2)))) '((1 2)))
    (equal (nth-row 0 (matrix-from-data '((1)(2)))) '((1)))
    (equal (nth-row 0 (matrix-from-data '((1 2)(3 4)))) '((1 2)))
    (equal (nth-row 1 (matrix-from-data '((1 2)(3 4)))) '((3 4)))
    (equal (nth-row 0 (matrix-from-data '((1 2)(3 4)(5 6)))) '((1 2)))
    (equal (nth-row 1 (matrix-from-data '((1 2)(3 4)(5 6)))) '((3 4)))
    (equal (nth-row 2 (matrix-from-data '((1 2)(3 4)(5 6)))) '((5 6)))

    ;; nth-col
    (equal (nth-col 0 (matrix-from-data '((1)))) '((1)))
    (equal (nth-col 0 (matrix-from-data '((1 2)))) '((1)))
    (equal (nth-col 1 (matrix-from-data '((1 2)))) '((2)))
    (equal (nth-col 0 (matrix-from-data '((1)(2)))) '((1)(2)))
    (equal (nth-col 0 (matrix-from-data '((1 2)(3 4)))) '((1)(3)))
    (equal (nth-col 1 (matrix-from-data '((1 2)(3 4)))) '((2)(4)))
    (equal (nth-col 0 (matrix-from-data '((1 2)(3 4)(5 6)))) '((1)(3)(5)))
    (equal (nth-col 1 (matrix-from-data '((1 2)(3 4)(5 6)))) '((2)(4)(6)))

    ;; []
    (compare-matrix ([] 0 0 (matrix-from-data '((1)(2)(3)))) (matrix-from-data '((1))))
    (compare-matrix ([] 0 1 (matrix-from-data '((1)(2)(3)))) (matrix-from-data '((1)(2))))
    (compare-matrix ([] 0 2 (matrix-from-data '((1)(2)(3)))) (matrix-from-data '((1)(2)(3))))
    (compare-matrix ([] 1 2 (matrix-from-data '((1)(2)(3)))) (matrix-from-data '((2)(3))))
    (compare-matrix ([] 0 0 (matrix-from-data '((1 1)(2 2)(3 3)))) (matrix-from-data '((1 1))))
    (compare-matrix ([] 0 1 (matrix-from-data '((1 1)(2 2)(3 3)))) (matrix-from-data '((1 1)(2 2))))
    (compare-matrix ([] 0 2 (matrix-from-data '((1 1)(2 2)(3 3)))) (matrix-from-data '((1 1)(2 2)(3 3))))
    (compare-matrix ([] 1 2 (matrix-from-data '((1 1)(2 2)(3 3)))) (matrix-from-data '((2 2)(3 3))))
  ))

(deftest test-matrix-modifications()
  (check
    ;; remove-col
    (compare-matrix (remove-col 0 (matrix-from-data '((1)))) (matrix-from-data '(()))) ; correct?
    (compare-matrix (remove-col 0 (matrix-from-data '((1 2)))) (matrix-from-data '((2))))
    (compare-matrix (remove-col 1 (matrix-from-data '((1 2 3)))) (matrix-from-data '((1 3))))
    (compare-matrix (remove-col 0 (matrix-from-data '((1 2 3)(4 5 6)))) (matrix-from-data '((2 3)(5 6))))
    (compare-matrix (remove-col 1 (matrix-from-data '((1 2 3)(4 5 6)))) (matrix-from-data '((1 3)(4 6))))
    (compare-matrix (remove-col 2 (matrix-from-data '((1 2 3)(4 5 6)))) (matrix-from-data '((1 2)(4 5))))

    ;; remove-row
    ;(compare-matrix (remove-row 0 (matrix-from-data '((1)))) (matrix-from-data '(()))) ; TODO
    (compare-matrix (remove-row 0 (matrix-from-data '((1)(2)(3)))) (matrix-from-data '((2)(3))))
    (compare-matrix (remove-row 1 (matrix-from-data '((1)(2)(3)))) (matrix-from-data '((1)(3))))
    (compare-matrix (remove-row 1 (matrix-from-data '((1)(2)(3)))) (matrix-from-data '((1)(3))))

    ;;prefix-const-val
    (compare-matrix (prefix-const-val 0 (matrix-from-data '((1)))) (matrix-from-data '((0 1))))
    (compare-matrix (prefix-const-val 0 (matrix-from-data '((1 2)))) (matrix-from-data '((0 1 2))))
    (compare-matrix (prefix-const-val 0 (matrix-from-data '((1)(2)))) (matrix-from-data '((0 1)(0 2))))
    (compare-matrix (prefix-const-val 0 (matrix-from-data '((1 2)(3 4)))) (matrix-from-data '((0 1 2)(0 3 4))))
  ))

(deftest test-dot-product ()
  (check
    (compare-matrix (dot (matrix-from-data '((1))) (matrix-from-data '((1)))) (matrix-from-data '((1))))
    (compare-matrix (dot (matrix-from-data '((1))) (matrix-from-data '((1 2)))) (matrix-from-data '((1 2))))
    (compare-matrix (dot (matrix-from-data '((1 2))) (matrix-from-data '((1)(2)))) (matrix-from-data '((5))))
    (compare-matrix (dot (matrix-from-data '((1 2)(3 4))) (matrix-from-data '((1 2)(3 4)))) (matrix-from-data '((7 10)(15 22))))
    (compare-matrix (dot (matrix-from-data '((1 2 3)(4 5 6))) (matrix-from-data '((1 2)(3 4)(5 6)))) (matrix-from-data '((22 28)(49 64))))
  ))

(deftest test-element-wise-operations ()
  (check
   ;; add
   (compare-matrix (add (matrix-from-data '((1))) (matrix-from-data '((1)))) (matrix-from-data '((2))))
   (compare-matrix (add (matrix-from-data '((1 2))) (matrix-from-data '((3 4)))) (matrix-from-data '((4 6))))
   (compare-matrix (add (matrix-from-data '((1)(2))) (matrix-from-data '((3)(4)))) (matrix-from-data '((4)(6))))
   (compare-matrix (add (matrix-from-data '((1 2)(3 4))) (matrix-from-data '((5 6)(7 8)))) (matrix-from-data '((6 8)(10 12))))

   ;; subtract
   (compare-matrix (subtract (matrix-from-data '((1))) (matrix-from-data '((1)))) (matrix-from-data '((0))))
   (compare-matrix (subtract (matrix-from-data '((1 2))) (matrix-from-data '((2 4)))) (matrix-from-data '((-1 -2))))
   (compare-matrix (subtract (matrix-from-data '((1)(2))) (matrix-from-data '((3)(5)))) (matrix-from-data '((-2)(-3))))
   (compare-matrix (subtract (matrix-from-data '((1 2)(3 4))) (matrix-from-data '((4 7)(5 10)))) (matrix-from-data '((-3 -5)(-2 -6))))

   ;; add-value
   (compare-matrix (add-value 1 (matrix-from-data '((1)))) (matrix-from-data '((2))))
   (compare-matrix (add-value 2 (matrix-from-data '((1 2)))) (matrix-from-data '((3 4))))
   (compare-matrix (add-value 3 (matrix-from-data '((1 2)(3 4)))) (matrix-from-data '((4 5)(6 7))))

   ;; multiply (by constant)
   (compare-matrix (multiply 2 (matrix-from-data '((1)))) (matrix-from-data '((2))))
   (compare-matrix (multiply 3 (matrix-from-data '((1 2)))) (matrix-from-data '((3 6))))
   (compare-matrix (multiply 3 (matrix-from-data '((1)(2)))) (matrix-from-data '((3)(6))))
   (compare-matrix (multiply 4 (matrix-from-data '((1 2)(3 4)))) (matrix-from-data '((4 8)(12 16))))

   ;; value-matrix-subtract
   (compare-matrix (value-matrix-subtract 0 (matrix-from-data '((1)))) (matrix-from-data '((-1))))
   (compare-matrix (value-matrix-subtract 1 (matrix-from-data '((1)(2)))) (matrix-from-data '((0)(-1))))
   (compare-matrix (value-matrix-subtract 2 (matrix-from-data '((1 2)(3 4)))) (matrix-from-data '((1 0)(-1 -2))))

   ;; matrix-mult
   (compare-matrix (matrix-mult  (matrix-from-data '((1))) (matrix-from-data '((-1)))) (matrix-from-data '((-1))))
   (compare-matrix (matrix-mult  (matrix-from-data '((1)(2))) (matrix-from-data '((2)(8)))) (matrix-from-data '((2)(16))))
   (compare-matrix (matrix-mult  (matrix-from-data '((1 2)(3 4))) (matrix-from-data '((2 3)(9 0)))) (matrix-from-data '((2 6)(27 0))))
   (compare-matrix (matrix-mult  (matrix-from-data '((1 2 3)(4 5 6)(7 8 9))) (matrix-from-data '((0 0 0)(7 6 5)(1 2 3)))) (matrix-from-data '((0 0 0)(28 30 30)(7 16 27))))

   ;; subtract-row
   (compare-matrix (subtract-row  (matrix-from-data '((1))) (matrix-from-data '((-1)))) (matrix-from-data '((2))))
   (compare-matrix (subtract-row  (matrix-from-data '((1)(2))) (matrix-from-data '((1)))) (matrix-from-data '((0)(1))))
   (compare-matrix (subtract-row  (matrix-from-data '((1 2)(3 4))) (matrix-from-data '((2 3)))) (matrix-from-data '((-1 -1)(1 1))))
   (compare-matrix (subtract-row  (matrix-from-data '((1 2 3)(4 5 6)(7 8 9))) (matrix-from-data '((1 5 9)))) (matrix-from-data '((0 -3 -6)(3 0 -3)(6 3 0))))

   ;; power
   (compare-matrix (power  3 (matrix-from-data '((2)))) (matrix-from-data '((8))))
   (compare-matrix (power  2 (matrix-from-data '((1 2)))) (matrix-from-data '((1 4))))
   (compare-matrix (power  2 (matrix-from-data '((1 2)(3 4)))) (matrix-from-data '((1 4)(9 16))))
   (compare-matrix (power  2 (matrix-from-data '((1 2 3)(4 5 6)(7 8 9)))) (matrix-from-data '((1 4 9)(16 25 36)(49 64 81))))
  ))

(deftest test-aggregating-functions ()
  (check
   ;; sum-rows
   (compare-matrix (sum-rows (matrix-from-data '((1)))) (matrix-from-data '((1))))
   (compare-matrix (sum-rows (matrix-from-data '((1 2)))) (matrix-from-data '((3))))
   (compare-matrix (sum-rows (matrix-from-data '((1 2 3)))) (matrix-from-data '((6))))
   (compare-matrix (sum-rows (matrix-from-data '((1 2 3)(4 5 6)))) (matrix-from-data '((6)(15))))

   ;; sum-cols
   (compare-matrix (sum-cols (matrix-from-data '((1)))) (matrix-from-data '((1))))
   (compare-matrix (sum-cols (matrix-from-data '((1 2 3)))) (matrix-from-data '((1 2 3))))
   (compare-matrix (sum-cols (matrix-from-data '((1 2 3)(1 2 3)))) (matrix-from-data '((2 4 6))))
   (compare-matrix (sum-cols (matrix-from-data '((1 2 3)(1 2 3)(1 2 3)))) (matrix-from-data '((3 6 9))))
  ))

(deftest test-sort-functions ()
  (check
   ;; arg-sort-col-mat
   (equal (arg-sort-col-mat (matrix-from-data '((1)))) '(0))
   (equal (arg-sort-col-mat (matrix-from-data '((1)(0)))) '(1 0))
   (equal (arg-sort-col-mat (matrix-from-data '((5)(4)(3)(2)(1)(0)))) '(5 4 3 2 1 0))
   ))

(deftest test-mathematical-functions ()
  (check
   ;; sigmoid-base
   (equal (/ 1 2) (sigmoid-base 0))
   (< (abs (- (sigmoid-base 10) 1)) 1E-4)
   (< (abs (- (sigmoid-base -10) 0)) 1E-4)
  ))

(deftest test-value-extremes ()
  (check
    ;; nth-col-max
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
    (equal (nth-col-min 0 (matrix-from-data '((1 2 3)))) 1)
    (equal (nth-col-min 1 (matrix-from-data '((1 2 3)))) 2)
    (equal (nth-col-min 2 (matrix-from-data '((1 2 3)))) 3)
    (equal (nth-col-min 0 (matrix-from-data '((1 2 3)(4 5 6)))) 1)
    (equal (nth-col-min 1 (matrix-from-data '((1 2 3)(4 5 6)))) 2)
    (equal (nth-col-min 2 (matrix-from-data '((2 2 3)(4 5 6)))) 3)
    (equal (nth-col-min 0 (matrix-from-data '((7 8 9)(1 2 3)(4 5 6)))) 1)
    (equal (nth-col-min 1 (matrix-from-data '((7 8 9)(1 2 3)(4 5 6)))) 2)
    (equal (nth-col-min 2 (matrix-from-data '((7 8 9)(2 2 3)(4 5 6)))) 3)
  ))

(deftest test-all ()
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
  ))
