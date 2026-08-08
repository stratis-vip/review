# Review Testing Library 

A small testing library, written for practicing.

# Overview 

A testing library, based on fiveam, that i create trying to understand better the testing mechanism. 

# Usage 

1. Every test file must have the usual (defpackage ... ) section or a package.lisp file where the package will be defined
2. (clear-suites) are mandatory to initialize library.
3. We need to define at least one suite. Every suite can have from 0 to as-many-as-we-want checks. 
4. For now there are 3 checks 
```lisp
(defmacro check (form)) ; checks if FORM is T
(defmacro check-not (form)) ; checks if FORM is NIL
(defmacro raise-error (form &optional (expected-error t)) ; checks if FORM will raise the - optional - expected error
``` 

## example
 
 ```lisp
 ;;;; tests.lisp --- the main tests for Review Testing Library
;;;; 
;;;; Code: 

(defpackage :review/tests 
  (:use :cl :review)        ; this is where we call review library
  (:export 
    :run-tests
    )
  )

(in-package :review/tests)

(clear-suites)             ; needed for initialization

(defsuite first-suite)     ; creation of suite "first-suite"
(defsuite second-suite)

(in-suite first-suite)     ; all tests below - till next in-suite are in first-suite

(test basics 
  (check t)
  (Check (= 1 1))
  (check (equal (list 1 3) '(1 3)))
  )

(test another-one-basic
  (check (null nil))
  (check-not nil)
  (check-not (= 2 3))
  )

(defun not-really-a-list (x) x)  ; helper function 

(test errors
  (raise-error (car (not-really-a-list "alfa")) type-error)
  (raise-error (car (not-really-a-list "bita")) simple-error)
  )

(defun run-tests ()
  (format t "to be concluded")) 

;;;; tests.lisp code ends here.
```

# Documentation
## Global variables (not exported)
### \*registry-suite\*
```lisp
(defvar *registry-suite* (make-hash-table :test #'eq)
  "Keeps all suites with the tests and their checks.")
```
### \*current-suite\*
```lisp
(defvar *current-suite* nil
  "The current suite. A defstruct SUITE")
```



## Macros: 

  ### defsuite
  ```lisp
  (defmacro defsuite (name)
  "Creates a SUITE structure with the NAME and add it to the *registry-suite* hash table.
NAME must be UNQUOTED symbol"
  ...)
  ```
  ### in-suite
  ```lisp
  (defmacro in-suite (name)  ; name is an UNQUOTED symbol
   "Sets the *CURRENT-SUITE* global variable to the suite with name NAME"
   ...)
  ```
  ### test
  ```lisp
  (defmacro test (name &body body) ; name is an UNQUOTED symbol
 "Creates a TEST with 0 or more checks in body"                       
 ...)           
  ```

  ### check
  ```lisp
  (defmacro check (form)
    "Sets a CHECK in a TEST"
    ...)
  ```
  ### check-not
  ```lisp
  (defmacro check-not (form)
    "Sets a CHECK-NOT in a TEST"
    ...)
  ```
  ### raise-error
  ```lisp
  (defmacro raise-error (form &optional (expected-error t))
   "Evaluates FORM, expecting it to signal an error of type EXPECTED-ERROR
(or any error, if EXPECTED-ERROR is T or omitted). Returns a CHECK-STRUCT."
...)
  ```


  ## Functions 
  ### clear-suites
  ```lisp
(defun clear-suites ()
  "Resets the *-SUITE* variables"
  ...)
  ```
  ### run-test
  ```lisp
  (defun run-test (name &key (registry *registry-suite*))
  "Runs the TEST NAME"
  ...)
  ```
  ### run-suite
  ```lisp
  (defun run-suite (name &key (registry *registry-suite*))
"Runs the SUITE's NAME Tests."
...)
  ```
