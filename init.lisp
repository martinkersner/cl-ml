;;;; Martin Kersner, m.kersner@gmail.com
;;;; 2016/09/16

(push (namestring (ext:cd)) asdf:*central-registry*)
(push "plot/" asdf:*central-registry*)
(push "math/" asdf:*central-registry*)

(asdf:load-system :cl-ml)
(asdf:load-system :cl-plot)
