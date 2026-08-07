;;;; review.lisp --- the main file fo Review Testing Library 
;;;;
;;;; Code: 

(in-package :review)

(defvar *current-suite* nil)

(defvar *registry-suite* (make-hash-table :test #'eq))

(defstruct suite
  name
  (tests (make-hash-table :test #'eq)))

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
  (gethash name registry))

(defun register-suite (name &key (registry *registry-suite*))
  (check-type name symbol)
  (when (gethash name registry)
    (error "Suite ~S already exists" name))
  (setf (gethash name registry) (make-suite :name name)))

(defmacro defsuite (name)
  `(register-suite ',name)
)

(defmacro in-suite (name)
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
  `(register-test ',name
                  (list ,@body)))

(defun find-test (name &key (registry *registry-suite*))
  (loop for st being the hash-value of registry
      thereis (gethash name (suite-tests st)))
  )

(defun run-test (name &key (registry *registry-suite*))
  "Runs the TEST NAME"
  (check-type name symbol)
  (let ((current-test (find-test name :registry registry)))
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
		  do (run-test (test-struct-name st) :registry registry)
		     (when (test-struct-passed st) (incf passed))
		  )
	    (format t "~%Suite ~S: (~A)~%" name (if (= total  passed) "PASSED" "FAILED" ))
	    (format t "Total tests: ~d~%" total)
	    (format t "Failed tests: ~d~%" (- total passed))
	    
	    )
	  ))))

(defun clear-suites ()
  "Resets the *tests* variable"
  (setf *registry-suite* nil
	*registry-suite* (make-hash-table :test #'eq)
	*current-suite* nil))


;;;; review.lisp code ends here 
