;;;; review.lisp --- the main file fo Review Testing Library 
;;;;
;;;; Code: 

(in-package :review)

(defvar *current-suite* nil
  "The current suite. A defstruct SUITE")

(defvar *registry-suite* (make-hash-table :test #'eq)
  "Keeps all suites with the tests and their checks.")

(defstruct suite
  name
  (tests (make-hash-table :test #'eq))
  passed)

(defstruct test-struct
  name 
  checks ;; a simple list contains all checks!
  passed
  documentation 
  )

(defstruct check-struct
  form
  value
  passed
  type ;; :check or :error
  error-expected 
  error-raised
  )

(defun find-suite (name &key (registry *registry-suite*))
  "Returns the SUITE NAME, if exists at REGISTRY. Default registry is *registry-suite*"
  (gethash name registry))

(defun register-suite (name &key (registry *registry-suite*))
  "Register a SUITE with name NAME to the registry REGISTRY. Default registry is *registry-suite*. "
  (check-type name symbol)
  (when (gethash name registry)
    (error "Suite ~S already exists" name))
  (setf (gethash name registry) (make-suite :name name)))

(defmacro defsuite (name)
  "Creates a SUITE structure with the NAME and add it to the *registry-suite* hash table.
NAME must be UNQUOTED symbol"
  `(register-suite ',name)
)

(defmacro in-suite (name)
  "Sets the *CURRENT-SUITE* global variable to the suite with name NAME"
  `(let ((local-suite (find-suite ',name)))
     (unless local-suite
       (error "Unknown suite ~S" ',name))
     (setf *current-suite* local-suite)))

(defun bool-atom-p (form) 
  "Checks if FORM is t or nil"
  (or (eq form t) (eq form nil)))


(defmacro make-correct-check (form)
  "Evaluates FORM once. Signals an error if FORM raises an error, or if
its value isn't exactly T or NIL. Otherwise evaluates to that value."
  (let ((result (gensym "RESULT"))
        (errored (gensym "ERRORED")))
    `(let* ((,errored nil)
            (,result (handler-case ,form
                       (error () (setf ,errored t) nil))))
       (when (or ,errored (not (bool-atom-p ,result)))
         (error "Form ~S must be T or NIL, and must evaluate without error!" ',form))
       ,result)))

(defmacro prepare-check (form &key (is-true t))
  "Template check macro, so don't need to repeat code for different type of checks."
  `(let ((value (make-correct-check ,form)))
     (make-check-struct 
      :form ',form
      :value value
      :passed ,(if is-true  
                   '(not (null value ))
                 '(null value))
      :type :check
      )) 
  )

(defmacro check (form)
  "Sets a CHECK in a TEST"
  `(prepare-check ,form )) 

(defmacro check-not (form)
  `(prepare-check ,form :is-true nil)) 

(defmacro raise-error (form &optional (expected-error t))
  "Evaluates FORM, expecting it to signal an error of type EXPECTED-ERROR
(or any error, if EXPECTED-ERROR is T or omitted). Returns a CHECK-STRUCT."
  (let ((form-var (gensym "FORM-"))
        (condition-var (gensym "CONDITION-"))
        (expected-str (if (eq expected-error t) "any error" (string expected-error))))
    `(let ((,form-var (lambda () ,form)))
       (handler-case
           (progn
             (funcall ,form-var)
             (make-check-struct
              :form ',form
              :value nil
              :passed nil
              :type :error
              :error-expected ,expected-str
              :error-raised nil))
         (error (,condition-var)
           (let ((passed-p ,(if (eq expected-error t)
                                 t
                                 `(typep ,condition-var ',expected-error))))
             (make-check-struct
              :form ',form
              :value ,condition-var
              :passed passed-p
              :type :error
              :error-expected ,expected-str
              :error-raised (type-of ,condition-var))))))))

(defun register-test (name checks)
  "Register the test NAME with it's relevant CHECKS to the *current-suite* variable."
  (check-type name symbol)

  (unless *current-suite*
    (error "There is not a current suite set with IN-SUITE"))

  (when (gethash name (suite-tests *current-suite*))
    (error "There is already a test ~S in ~S suite."
           name
           (suite-name *current-suite*)))
  (let (docstring)
    (when (and (stringp (first checks)) (rest checks))
      (setf docstring (pop checks)))
    
    (setf (gethash name (suite-tests *current-suite*))
	  (make-test-struct
	   :name name
	   :checks checks
	   :passed (every #'check-struct-passed checks)
	   :documentation docstring))))

(defmacro test (name &body body)
  "Creates a TEST with 0 or more checks in body"
  `(register-test ',name
                  (list ,@body)))

(defun find-test (name &key suite (registry *registry-suite*))
 "If SUITE is given, searches only within it. Otherwise searches every suite in REGISTRY."
  (if suite
      (gethash name (suite-tests suite))
      (loop for st being the hash-value of registry
	    thereis (gethash name (suite-tests st)))))
  

(defun run-test (name &key suite (registry *registry-suite*))
  "Runs the TEST NAME"
  (check-type name symbol)
  (let ((current-test (find-test name :suite suite :registry registry)))
    (if current-test
	(let* ((check-list (test-struct-checks current-test))
	       (total-checks (length check-list))
	       (passed-cheks (loop for ch in check-list 
				   count (check-struct-passed ch)))
	       (failed-checks (- total-checks passed-cheks))
	       (test-passed (= total-checks passed-cheks))
		 
		 )
	    (progn 
	      (format t "~%Test: ~a (~A)~%" name (if test-passed "PASSED" "FAILED"))
	      (format t "Total cheks: ~d~%" total-checks)
	      (unless test-passed 
                (progn  (format t "Passed: ~d~%~%" passed-cheks)
                        (format t "Failed: ~d~%" failed-checks)
                        (loop for failed-test in (remove-if #'check-struct-passed check-list)
			      do (if (eq (check-struct-type failed-test) :check)
				     (format t "✗ ~S expected to be true~%" 
					     (check-struct-form failed-test))
				     (format t "✗ ~S expected ~S - get ~S~%" 
					     (check-struct-form failed-test)
					     (check-struct-error-expected failed-test)
					     (check-struct-error-raised failed-test))))))))
	(error "No test with name ~S exists" name))))

(defun run-suite (name &key (registry *registry-suite*))
"Runs the SUITE's NAME Tests."
  (check-type name symbol)
  
  (let ((current-suite (find-suite name :registry registry)))
    (unless current-suite
      (error "Suite ~S not exists!" name))
    (let* ((total (hash-table-count (suite-tests current-suite)))
	   (passed 0))
      (if (zerop total)
	  (format t "No tests in suite ~S" name)
	  (progn
	    (loop for st being the hash-value of (suite-tests current-suite)
		  do (run-test (test-struct-name st) :suite current-suite :registry registry)
		     (when (test-struct-passed st) (incf passed))
		  )
	    (setf (suite-passed current-suite) (= total passed))

	    (format t "~%Suite ~S: (~A)~%" name (if (= total  passed) "PASSED" "FAILED" ))
	    (format t "Total tests: ~d~%" total)
	    (format t "Failed tests: ~d~%" (- total passed))
	    
	    )
	  ))))

(defun clear-suites ()
  "Resets the *-SUITE* variables"
  (setf *registry-suite* (make-hash-table :test #'eq)
	*current-suite* nil))

(defun run-tests (&key (registry *registry-suite*))
  "Run all Suites"
  (if (zerop (hash-table-count registry))
      (format t "No suites and tests YET!")
      (let ((passed 0)
	    (total (hash-table-count registry)))
	(loop for name being the hash-key of registry using (hash-value suite)
	      do (run-suite name :registry registry)
		 (when (suite-passed suite) (incf passed))
	      )
	(format t "~%---------------------~%Total suites: ~d " total)
	(if (= passed total)
	    (format t "(ALL PASSED)~%")
	    (format t "(FAILED)~%Passed: ~d~%Failed: ~d~%" passed (- total passed)))
	)

      
      ))

;;;; review.lisp code ends here 
