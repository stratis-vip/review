;;;; tests.lisp --- the main tests for Review Testing Library
;;;; 
;;;; Code: 

(in-package :review/tests)

(clear-suites)

(defsuite first-suite)
(defsuite second-suite)
(in-suite first-suite)

(test dokimi 
  (check t)
  (Check (= 1 1))
  (check (equal (list 1 3) '(1 3)))
  )

(test alli-mia-dokimi
  (check (null nil))
  (check-not nil)
  (check-not (= 2 3))
  )

(defun not-really-a-list (x) x)

(test errors
  (raise-error (car (not-really-a-list "alfa")) type-error)
  (raise-error (car (not-really-a-list "bita")) simple-error)
  )

(defun run-all-tests ()
  (run-tests)) 
;;;; tests.lisp code ends here.
