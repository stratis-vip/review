# Review Testing Library

A small testing library, written for practicing.

# Overview

A testing library, based on fiveam, that i create trying to understand better the testing mechanism.

# Usage

## Installation

You need to clone this repo to a position that common lisp recognize (usually to ~/common-lisp/).
After that

```lisp
(asdf:load-system :review)       ;; review Library
(asdf:load-system :review/tests) ;; Tests of review Library
(asdf:test-system :review/tests) ;; Run all tests
```

1. Every test file must have the usual (defpackage ... ) section or a package.lisp file where the package will be defined
2. You must :use :review
3. (clear-suites) are mandatory to initialize library. Best solution is to create a fil test-setup.lisp that loads first of all tests and (clear-suites) only then. 
4. We need to define at least one suite. Every suite can have from 0 to as-many-as-we-want checks.
5. For now there are 5 checks

```lisp
(defmacro check (form)) ; checks if FORM is T
(defmacro check-not (form)) ; checks if FORM is NIL
(defmacro raise-error (form &optional (expected-error t)) ; checks if FORM will raise the - optional - expected error
(defmacro check-for-all (variables &body expressions));Checks all EXPRESSIONS for every combination of VARIABLES.
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
(defsuite third-suite)

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

(in-suite second-suite)

(test dokimi
 (check t))

(check-for-all ((a -5 10)
               (b -5 10)
		        (c -5 10))
   (= (+ a c) (+ c a ))
   (= (+ a (+ b c)) (+ (+ a b) c))
   (= (* a b) ( * b a))
   (= (* a (* b c) ) ( * ( * a b) c))))

(check
 (let ((a 3) (b 4) (c 5))
 (implies (and (< a b) (< b c))
          (< a c))))

(defun run-all-tests ()
 (run-tests))

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

### check-for-all

```lisp
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
...)
```

### implies

```lisp
(defmacro implies (condition consequence)
"Return T when the CONSEQUENCE is T based to the CONDITION
Must be inside a CHECK block."
 `(or (not ,condition)
      ,consequence))
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
(defun run-test (name &key suite (registry *registry-suite*))
"Runs the TEST NAME in SUITE suit (if given)"
...)
```

### run-suite

```lisp
(defun run-suite (name &key (registry *registry-suite*))
"Runs the SUITE's NAME Tests."
...)
```

### run-tests

```lisp
(defun run-tests (&key (registry *registry-suite*))
"Run all Suites"
...)
```

## Helper Functions

### return-t

```lisp
(defun return-t (x)
  "Returns T regardless of the value of X.

X is intentionally ignored. This function is useful as a predicate
when a condition should always be considered true at runtime, while
avoiding compile-time evaluation of the condition."
  (declare (ignore x))
  t)
```

### return-nil

```lisp
(defun return-nil (x)
  "Returns NIL regardless of the value of X.

X is intentionally ignored. This function is useful as a predicate
when a condition should always be considered false at runtime, while
avoiding compile-time evaluation of the condition."
  (declare (ignore x))
  nil)
```

### return-value
```lisp
(defun return-value (x)
  "Returns the value of X unchanged.

This function can be used to defer evaluation of a value until
runtime, particularly when a value must not be inspected or evaluated
during macro expansion or compile-time processing."
  (funcall (lambda (x) x ) x))
````
