;;;; Martin Kersner, m.kersner@gmail.com
;;;; 2016/09/16

(load "asdf/build/asdf")
(push (namestring (ext:cd)) asdf:*central-registry*)
(asdf:load-system :lispml)
