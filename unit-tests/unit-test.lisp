;;;; by http://www.gigamonkeys.com/book/practical-building-a-unit-test-framework.html

(in-package :lispml)

(defvar *test-name* nil)

;;; If macro WITH-GENSYSM is locked type continue and hit enter. (clisp)
(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))

(defmacro deftest (name parameters &body body)
  "Define a test function. Within a test function we can call
   other test functions or use 'check' to run individual test
   cases."
  `(defun ,name ,parameters
    (let ((*test-name* (append *test-name* (list ',name))))
      ,@body)))

(defmacro check (&body forms)
  "Run each expression in 'forms' as a test case."
  `(combine-results
    ,@(loop for f in forms collect `(report-result ,f ',f))))

(defmacro combine-results (&body forms)
  "Combine the results (as booleans) of evaluating 'forms' in order."
  (with-gensyms (result)
    `(let ((,result t))
      ,@(loop for f in forms collect `(unless ,f (setf ,result nil)))
      ,result)))

(defun report-result (result form)
  "Report the results of a single test case. Called by 'check'."
  (format t "~:[FAIL~;PASS~] ... ~a: ~a~%" result *test-name* form)
  result)

(defun unit-test-coverage (namespace unit-test-namespace unit-test-name)
  (let ((namespace-size (length namespace))
        (unit-test-namespace-size 0)
        (coverage 0)
        (not-covered '()))

    (mapcar #'(lambda (name) (if (member name unit-test-namespace)
                               (setf unit-test-namespace-size
                                     (1+ unit-test-namespace-size))
                               (push name not-covered)))
            namespace)

    (setf coverage (* (/ unit-test-namespace-size namespace-size) 100))

    (print (format nil "~:@(~a~) unit tests coverage ~f %"
            unit-test-name
            coverage))

    (if (> (length not-covered) 0)
      (print (format nil "Missing tests for ~{~a~#[~;, and ~:;, ~]~}" not-covered))))

  T)
