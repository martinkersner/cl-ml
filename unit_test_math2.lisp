;;;; Martin Kersner, m.kersner@gmail.com
;;;; 2016/04/27 

;;; TODO check error conditions in tests

(load "unit_test")
(load "math2")

(deftest test-generate-matrix ()
  (check
    ;; empty-matrix
    (compare-matrix (empty-matrix 1 1) (make-matrix :rows 1 :cols 1 :data '((nil))))
    (compare-matrix (empty-matrix 2 1) (make-matrix :rows 2 :cols 1 :data '((nil)(nil))))
    (compare-matrix (empty-matrix 1 2) (make-matrix :rows 1 :cols 2 :data '((nil nil))))
    (compare-matrix (empty-matrix 2 2) (make-matrix :rows 2 :cols 2 :data '((nil nil)(nil nil))))
    (compare-matrix (empty-matrix 2 3) (make-matrix :rows 2 :cols 3 :data '((nil nil nil)(nil nil nil))))

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

(deftest test-all ()
  (combine-results
    (test-generate-matrix)
    (test-transpose-matrix)
   ))
