;;;; Martin Kersner, m.kersner@gmail.com
;;;; 2016/03/13
;;;;
;;;; Simple CSV reader

;;; Modified http://batsov.com/articles/2011/04/30/parsing-numbers-from-string-in-lisp/
(defun str2list (str)
  (let ((str-clean (substitute #\SPACE #\, str))) ; remove commas

    (when str-clean
      nil
      (with-input-from-string (in str-clean)
        (loop for x = (read in nil nil) while x collect x)))))

(defun read-csv (csv-file)
  (let ((in (open csv-file :if-does-not-exist nil)))
    (when in
      (let ((csv-list (loop for line = (read-line in nil)
                            for tmp-csv-list = (str2list line)
                            while line
                            do() collect tmp-csv-list)))

        (close in)
        csv-list))))
