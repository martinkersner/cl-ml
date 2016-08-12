;;;; Martin Kersner, m.kersner@gmail.com
;;;; 2016/08/12 

(defpackage #:lisp-ml
  (:use :cl :asdf))

(in-package :lisp-ml)

(defsystem lisp-ml
  :name "lisp-ml"
  :version "0.0.1"
  :author "Martin Kersner, <m.kersner@gmail.com>"
  :long-description "Machine Learning library for Common Lisp"
  :serial t
  :components ((:file "defpackage")
               (:file "random")
               (:file "list")
               (:file "matrix"))

               (:module linear-regression)
                  :components ((:file "linear-regression")
                               (:file "linear-regression-example")))
