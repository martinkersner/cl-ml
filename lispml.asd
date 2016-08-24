;;;; Martin Kersner, m.kersner@gmail.com
;;;; 2016/08/12 

(asdf:defsystem :lispml
  :name    "lispml"
  :version "0.0.1"
  :author  "Martin Kersner, <m.kersner@gmail.com>"
  :long-description "Machine Learning library for Common Lisp"
  :serial t
  :components ((:file "package")
               (:file "math")
               (:file "random")
               (:file "list")
               (:file "matrix")
               (:file "feature-extraction-and-preprocessing")
               (:file "csv-reader")

               (:module k-nearest-neighbors
                  :components ((:file "knn")))

               (:module linear-regression
                  :components ((:file "linear-regression")))

               (:module decision-trees
                  :components ((:file "id3")))

               (:module logistic-regression
                  :components ((:file "logistic-regression")))

               (:module neural-networks
                  :components ((:file "nn")))

               (:module unit-tests
                  :components ((:file "unit-test")
                               (:file "unit-test-list")
                               (:file "unit-test-math")
                               (:file "unit-test-matrix")))))
