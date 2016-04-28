;;;; Martin Kersner, m.kersner@gmail.com
;;;; 2016/04/27 

(load "unit_test")
(load "math2")

(deftest test-generate-matrix ()
  (check
    (compare-matrix (empty-matrix 1 1) (make-matrix :rows 1 :cols 1 :data '((nil)))) 
    ))

(deftest test-transpose-matrix ()
  (check
    (compare-matrix (transpose (make-matrix :rows 1 :cols 1 :data '((1))))
                    (make-matrix :rows 1 :cols 1 :data '((1))))

    ;(equal (transpose '((1 2))) '((1)(2)))
    ;(equal (transpose '((1 2)(3 4))) '((1 3)(2 4)))
    ))

(deftest test-all ()
  (combine-results
    (test-generate-matrix)
    (test-transpose-matrix)
   ))
