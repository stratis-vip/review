;;;; review.lisp --- the main file for Review Testing Library
;;;;
;;;; Code:

(in-package :review)


;;; ----------------------------------------------------------------------
;;; Global state
;;; ----------------------------------------------------------------------

(defvar *current-suite* nil
  "The current suite. A DEFSTRUCT SUITE.")

(defvar *registry-suite* (make-hash-table :test #'eq)
  "Keeps all suites with the tests and their checks.")

(defvar *current-checks* :not-running
  "Όσο τρέχει το σώμα ενός TEST, δεσμεύεται δυναμικά σε μια λίστα από
CHECK-STRUCT. Εκτός running test, είναι :NOT-RUNNING.")


;;; ----------------------------------------------------------------------
;;; Check management
;;; ----------------------------------------------------------------------

(defun add-check (check-struct)
  "Καταγράφει CHECK-STRUCT στο τρέχον test.
Σφάλμα αν δεν τρέχει test."

  (when (eq *current-checks* :not-running)
    (error
     "CHECK, CHECK-NOT, RAISE-ERROR, ή CHECK-FOR-ALL
      χρησιμοποιήθηκε εκτός running TEST."))

  (push check-struct *current-checks*)
  check-struct)


;;; ----------------------------------------------------------------------
;;; Data structures
;;; ----------------------------------------------------------------------

(defstruct suite
  name
  (tests (make-hash-table :test #'eq))
  passed)


(defstruct test-struct
  name
  body-fn
  checks
  passed
  documentation)


(defstruct check-struct
  form
  value
  passed
  type                         ; :check or :error
  expected
  error-expected
  error-raised
  counterexample
  attempts)


;;; ----------------------------------------------------------------------
;;; Suites
;;; ----------------------------------------------------------------------

(defun find-suite (name &key (registry *registry-suite*))
  "Returns the SUITE NAME, if exists at REGISTRY."

  (gethash name registry))


(defun register-suite (name &key (registry *registry-suite*))
  "Register a SUITE with NAME to REGISTRY."

  (check-type name symbol)

  (when (gethash name registry)
    (error "Suite ~S already exists" name))

  (setf (gethash name registry)
        (make-suite :name name)))


(defmacro defsuite (name)
  "Creates a SUITE structure with NAME and adds it to *REGISTRY-SUITE*."

  `(register-suite ',name))


(defmacro in-suite (name)
  "Sets *CURRENT-SUITE* to the suite with NAME."

  `(let ((local-suite (find-suite ',name)))
     (unless local-suite
       (error "Unknown suite ~S" ',name))
     (setf *current-suite* local-suite)))


;;; ----------------------------------------------------------------------
;;; Basic checks
;;; ----------------------------------------------------------------------

(defun bool-atom-p (form)
  "Checks if FORM is T or NIL."

  (or (eq form t)
      (eq form nil)))


(defmacro make-correct-check (form)
  "Evaluates FORM once.

Signals an error if FORM raises an error, or if its value
isn't exactly T or NIL.

Otherwise evaluates to that value."

  (let ((result (gensym "RESULT"))
        (errored (gensym "ERRORED")))

    `(let* ((,errored nil)
            (,result
              (handler-case
                  ,form
                (error ()
                  (setf ,errored t)
                  nil))))

       (when (or ,errored
                 (not (bool-atom-p ,result)))
         (error
          "Form ~S must be T or NIL, and must evaluate without error!"
          ',form))

       ,result)))


(defmacro prepare-check (form &key (is-true t))
  `(let ((value (make-correct-check ,form)))
     (add-check
      (make-check-struct
       :form ',form
       :value value
       :passed ,(if is-true
                    '(not (null value))
                    '(null value))
       :type :check
       :expected ,(if is-true
                      :true
                      :false)))))


(defmacro check (form)
  "Sets a CHECK in a TEST."

  `(prepare-check ,form))


(defmacro check-not (form)
  "Sets a CHECK-NOT in a TEST."

  `(prepare-check ,form :is-true nil))

(defmacro implies (condition consequence)
  "Return T when the CONSEQUENCE is T based to the CONDITION"
  `(or (not ,condition)
       ,consequence))

;;; ----------------------------------------------------------------------
;;; Error checks
;;; ----------------------------------------------------------------------

(defmacro raise-error (form &optional (expected-error t))
  (let ((form-var (gensym "FORM-"))
        (condition-var (gensym "CONDITION-"))
        (expected-str
          (if (eq expected-error t)
              "any error"
              (string expected-error))))

    `(let ((,form-var (lambda () ,form)))

       (add-check
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

            (let ((passed-p
                    ,(if (eq expected-error t)
                         t
                         `(typep ,condition-var
                                 ',expected-error))))

              (make-check-struct
               :form ',form
               :value ,condition-var
               :passed passed-p
               :type :error
               :error-expected ,expected-str
               :error-raised
               (type-of ,condition-var)))))))))


;;; ----------------------------------------------------------------------
;;; CHECK-FOR-ALL
;;; ----------------------------------------------------------------------

(defmacro check-expression (expression)
  "Evaluates EXPRESSION as a REVIEW boolean expression.

The expression must evaluate to exactly T or NIL and must not
signal an error.

No EVAL is used."

  `(make-correct-check ,expression))


(defmacro check-for-all (variables &body expressions)
  "Checks all EXPRESSIONS for every combination of VARIABLES.

Each expression produces its own CHECK-STRUCT.

Each CHECK-STRUCT stores:
  - the expression,
  - whether it passed,
  - the number of attempts,
  - the first counterexample, if any.

All expressions are tested independently over all combinations.

No EVAL is used."

  (let ((done (gensym "DONE-"))
        (states
          (loop for expression in expressions
                collect
                (list expression
                      (gensym "PASSED-")
                      (gensym "ATTEMPTS-")
                      (gensym "COUNTEREXAMPLE-")))))

    (unless expressions
      (error "CHECK-FOR-ALL requires at least one expression."))

    (labels
        ((generate-leaf ()

           `(progn

              ;; Test every expression for this combination.
              ,@(loop
                  for state in states
                  for expression = (first state)
                  for passed = (second state)
                  for attempts = (third state)
                  for counterexample = (fourth state)

                  collect
                  `(progn
                     (incf ,attempts)

                     (unless (check-expression ,expression)

                       ;; Keep the first counterexample only.
                       (unless ,counterexample
                         (setf
                          ,counterexample
                          (list
                           ,@(loop
                               for variable-spec in variables
                               for variable = (first variable-spec)
                               collect
                               `(cons ',variable ,variable)))))

                       (setf ,passed nil))))

              ;; If every expression has already failed,
              ;; there is nothing more useful to test.
              (when
                  (and
                   ,@(loop
                       for state in states
                       collect
                       `(not ,(second state))))

                (return-from ,done nil))))


         (generate-loops (remaining)

           (if (null remaining)

               (generate-leaf)

               (let ((variable
                       (first (first remaining)))
                     (from
                       (second (first remaining)))
                     (to
                       (third (first remaining))))

                 `(loop
                    for ,variable from ,from to ,to
                    do
                    ,(generate-loops
                      (rest remaining)))))))

      `(let
           ,(loop
              for state in states
              append
              `((,(second state) t)
                (,(third state) 0)
                (,(fourth state) nil)))

         (block ,done
           ,(generate-loops variables))

         ;; One CHECK-STRUCT per expression.
         ,@(loop
             for state in states
             for expression = (first state)
             for passed = (second state)
             for attempts = (third state)
             for counterexample = (fourth state)

             collect
             `(add-check
               (make-check-struct
                :form ',expression
                :value ,passed
                :passed ,passed
                :type :check
                :expected :true
                :counterexample ,counterexample
                :attempts ,attempts)))))))

;;; ----------------------------------------------------------------------
;;; Tests
;;; ----------------------------------------------------------------------

(defun register-test (name docstring body-fn)
  "Registers TEST NAME without executing its body."

  (check-type name symbol)
  (check-type body-fn function)

  (unless *current-suite*
    (error
     "There is not a current suite set with IN-SUITE."))

  (when (gethash name
                 (suite-tests *current-suite*))

    (error
     "There is already a test ~S in ~S suite."
     name
     (suite-name *current-suite*)))

  (setf
   (gethash name
            (suite-tests *current-suite*))

   (make-test-struct
    :name name
    :body-fn body-fn
    :checks nil
    :passed nil
    :documentation docstring))

  name)


(defmacro test (name &body body)
  (let (docstring)

    (when (and (stringp (first body))
               (rest body))
      (setf docstring
            (pop body)))

    `(register-test
      ',name
      ,docstring
      (lambda ()
        ,@body))))


;;; ----------------------------------------------------------------------
;;; Tests lookup
;;; ----------------------------------------------------------------------

(defun find-test (name &key suite (registry *registry-suite*))
  "If SUITE is given, searches only within it.
Otherwise searches every suite in REGISTRY."

  (if suite

      (gethash name
               (suite-tests suite))

      (loop
        for st being the hash-value
          of registry
        thereis
        (gethash name
                 (suite-tests st)))))


;;; ----------------------------------------------------------------------
;;; Reporting
;;; ----------------------------------------------------------------------

(defun report-counterexample (counterexample)
  (format t "    Counterexample: ")

  (loop for (variable . value) in counterexample
        for first = t then nil
        do
          (unless first
            (format t ", "))
          (format t "~a = ~a" variable value))

  (terpri))

(defun report-result (check)

  (if (eq (check-struct-type check) :check)

      (let ((prefix
              (if (eq (check-struct-expected check) :false)
                  "NOT "
                  "")))

        (if (check-struct-passed check)

            (format t
                    "✓ ~a~S~%"
                    prefix
                    (check-struct-form check))

            (progn
              (format t
                      "✗ ~a~S expected ~a~%"
                      prefix
                      (check-struct-form check)
                      (if (eq (check-struct-expected check) :true)
                          "T"
                          "NIL"))))

        ;; CHECK-FOR-ALL attempts
        (when (and (numberp (check-struct-attempts check))
                   (plusp (check-struct-attempts check)))
          (format t
                  "    Tries: ~d~%"
                  (check-struct-attempts check)))

        ;; CHECK-FOR-ALL counterexample
        (when (check-struct-counterexample check)
          (report-counterexample
           (check-struct-counterexample check))))

      ;; :ERROR
      (if (check-struct-passed check)

          (format t
                  "✓ ~S raised ~S~%"
                  (check-struct-form check)
                  (check-struct-error-raised check))

          (format t
                  "✗ ~S expected ~S - got ~S~%"
                  (check-struct-form check)
                  (check-struct-error-expected check)
                  (check-struct-error-raised check)))))

;;; ----------------------------------------------------------------------
;;; Run one test
;;; ----------------------------------------------------------------------

(defun run-test (name &key suite (registry *registry-suite*))
  "Runs TEST NAME.

The test body is executed here, not during registration/loading."

  (check-type name symbol)

  (let ((current-test
          (find-test name
                     :suite suite
                     :registry registry)))

    (unless current-test
      (error
       "No test with name ~S exists"
       name))

    ;; ---------------------------------------------------------------
    ;; Re-run the test from scratch.
    ;; ---------------------------------------------------------------
    (let ((*current-checks* nil))

      ;; Execute the test body.
      (funcall (test-struct-body-fn current-test))

      ;; Save the checks generated by this run.
      (let ((check-list
              (nreverse *current-checks*)))

        (setf (test-struct-checks current-test)
              check-list)

        (let* ((total-checks
                 (length check-list))

               (passed-checks
                 (loop
                   for ch in check-list
                   count (check-struct-passed ch)))

               (failed-checks
                 (- total-checks
                    passed-checks))

               (test-passed
                 (= total-checks
                    passed-checks)))

          (setf (test-struct-passed current-test)
                test-passed)

          (format t
                  "~%Test: ~a (~A)~%"
                  name
                  (if test-passed
                      "PASSED"
                      "FAILED"))

          (format t
                  "Total checks: ~d~%"
                  total-checks)

	  (unless test-passed
	    (format t "Passed: ~d~%" passed-checks)
	    (format t "Failed: ~d~%" failed-checks))

	  (loop for check in check-list
		do
		   (report-result check))
	 

          test-passed)))))


;;; ----------------------------------------------------------------------
;;; Run suite
;;; ----------------------------------------------------------------------

(defun run-suite (name &key (registry *registry-suite*))
  "Runs the SUITE's NAME tests."

  (check-type name symbol)

  (let ((current-suite
          (find-suite name
                      :registry registry)))

    (unless current-suite
      (error
       "Suite ~S not exists!"
       name))

    (let* ((total
             (hash-table-count
              (suite-tests current-suite)))

           (passed 0))

      (if (zerop total)

          (format t
                  "No tests in suite ~S"
                  name)

          (progn

            (loop
              for st being
                the hash-value
                of (suite-tests current-suite)

              do
                 (run-test
                  (test-struct-name st)
                  :suite current-suite
                  :registry registry)

                 (when
                     (test-struct-passed st)
                   (incf passed)))

            (setf
             (suite-passed current-suite)
             (= total passed))

            (format t
                    "~%Suite ~S: (~A)~%"
                    name
                    (if (= total passed)
                        "PASSED"
                        "FAILED"))

            (format t
                    "Total tests: ~d~%"
                    total)

            (format t
                    "Failed tests: ~d~%"
                    (- total passed)))))))


;;; ----------------------------------------------------------------------
;;; Registry
;;; ----------------------------------------------------------------------

(defun clear-suites ()
  "Resets the *-SUITE* variables."

  (setf
   *registry-suite*
   (make-hash-table :test #'eq)

   *current-suite*
   nil))


(defun run-tests (&key (registry *registry-suite*))
  "Runs all suites."

  (if (zerop
       (hash-table-count registry))

      (format t
              "No suites and tests YET!")

      (let ((passed 0)
            (total
              (hash-table-count registry)))

        (loop
          for name being
            the hash-key
            of registry

            using
            (hash-value suite)

          do
             (run-suite
              name
              :registry registry)

             (when
                 (suite-passed suite)
               (incf passed)))

        (format t
                "~%---------------------~%Total suites: ~d "
                total)

        (if (= passed total)

            (format t
                    "(ALL PASSED)~%")

            (format t
                    "(FAILED)~%Passed: ~d~%Failed: ~d~%"
                    passed
                    (- total passed))))))


;;;; review.lisp ends here
