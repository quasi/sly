;;; sly-test-mocks.el --- Mock objects for SLY unit tests -*- lexical-binding: t; -*-

;; Copyright (C) 2024 SLY Contributors

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; This file provides mock objects and utilities for testing SLY
;; functionality without requiring an actual Lisp connection.
;;
;; WHAT THIS FILE PROVIDES:
;;
;; When testing SLY code that calls sly-eval, sly-eval-async, or checks
;; connection status, this module provides mocks so tests don't need a
;; real Lisp server running.  Mocks integrate with the test framework
;; to verify which functions were called and with what arguments.
;;
;; KEY UTILITIES:
;;
;; Mock Setup:
;;   - sly-test-with-mock-eval: Execute code with mocked sly-eval responses
;;   - sly-test-with-mock-connection: Set up mock connection without eval
;;
;; Mock Responses:
;;   - sly-test-mock--responses: Registry of (REQUEST . RESPONSE) pairs
;;   - sly-test-mock-eval: Mock implementation of sly-eval
;;   - sly-test-mock-eval-async: Mock implementation of sly-eval-async
;;
;; Mock Connection:
;;   - sly-test-mock-make-connection: Create a mock connection object
;;   - sly-test-mock-connected-p: Mock sly-connected-p
;;   - sly-test-mock-current-package: Mock sly-current-package
;;
;; Verification:
;;   - sly-test-mock-assert-called: Verify function was called
;;   - sly-test-mock-assert-called-with: Verify function was called with args
;;   - sly-test-mock--calls-to: Get all calls to a function
;;
;; USAGE PATTERN:
;;
;; (ert-deftest sly-example-unit--completion-with-mock-eval ()
;;   "Test completion when Lisp returns suggestions."
;;   (sly-test-with-mock-eval
;;       (((slynk:simple-completions \"foo\" \"CL-USER\")
;;         . ((\"foobar\" \"foobaz\") \"foo\")))
;;     (let ((result (sly-eval '(slynk:simple-completions \"foo\" \"CL-USER\"))))
;;       (should (equal '(\"foobar\" \"foobaz\") (car result))))))
;;
;; For complete examples, see doc/TESTING-UNIT-TESTS.md.

;;; Code:

(require 'cl-lib)

;; Forward declarations to avoid requiring sly.el during unit tests
(defvar sly-buffer-package)
(defvar sly-buffer-connection)

;;; Mock Response Registry
;;;
(defvar sly-test-mock--responses nil
  "Alist of (REQUEST . RESPONSE) for mock evaluations.
Each REQUEST is compared with `equal' to incoming sexp.")

(defvar sly-test-mock--call-log nil
  "List of calls made to mock functions, most recent first.
Each entry is (FUNCTION-NAME . ARGS).")

(defun sly-test-mock--reset ()
  "Reset all mock state."
  (setq sly-test-mock--responses nil
        sly-test-mock--call-log nil))

(defun sly-test-mock--log-call (fn &rest args)
  "Log a call to FN with ARGS."
  (push (cons fn args) sly-test-mock--call-log))

(defun sly-test-mock--calls-to (fn)
  "Return list of calls made to FN."
  (cl-remove-if-not (lambda (entry) (eq (car entry) fn))
                    sly-test-mock--call-log))

;;; Mock sly-eval Implementation
;;;
(defun sly-test-mock--find-response (sexp)
  "Find a mock response for SEXP.
Returns the response or signals an error if not found."
  (let ((entry (cl-find-if
                (lambda (pair)
                  (let ((pattern (car pair)))
                    (if (functionp pattern)
                        (funcall pattern sexp)
                      (equal pattern sexp))))
                sly-test-mock--responses)))
    (if entry
        (let ((response (cdr entry)))
          (if (functionp response)
              (funcall response sexp)
            response))
      (error "No mock response registered for: %S" sexp))))

(defun sly-test-mock-eval (sexp &optional _package _cancel-on-input _cancel-retval)
  "Mock implementation of `sly-eval'.
Looks up SEXP in `sly-test-mock--responses' and returns the response."
  (sly-test-mock--log-call 'sly-eval sexp)
  (sly-test-mock--find-response sexp))

(defun sly-test-mock-eval-async (sexp cont &optional _package)
  "Mock implementation of `sly-eval-async'.
Immediately calls CONT with the mock response for SEXP."
  (sly-test-mock--log-call 'sly-eval-async sexp)
  (funcall cont (sly-test-mock--find-response sexp)))

;;; Mock Connection
;;;
(defvar sly-test-mock--connection-counter 0
  "Counter for generating unique mock connection IDs.")

(cl-defstruct (sly-test-mock-connection
               (:constructor sly-test-mock-connection--create))
  "A mock SLY connection for testing."
  (id (cl-incf sly-test-mock--connection-counter))
  (pid 99999)
  (implementation-type "MockLisp")
  (implementation-name "mock")
  (implementation-version "1.0.0")
  (machine-type "test-machine")
  (features '(:mock-lisp))
  (package "CL-USER"))

(defun sly-test-mock-make-connection ()
  "Create a new mock connection."
  (sly-test-mock-connection--create))

;;; Mock Connection Functions
;;;
(defun sly-test-mock-connected-p ()
  "Mock implementation of `sly-connected-p'."
  t)

(defun sly-test-mock-current-connection ()
  "Mock implementation of `sly-current-connection'."
  'mock-connection)

(defun sly-test-mock-current-package ()
  "Mock implementation of `sly-current-package'."
  (or (bound-and-true-p sly-buffer-package) "CL-USER"))

;;; Setup Macro
;;;
(defmacro sly-test-with-mock-eval (responses &rest body)
  "Execute BODY with mock evaluation returning RESPONSES.
RESPONSES is an alist of (SEXP . RESULT) pairs.
SEXP can be a literal form or a predicate function.
RESULT can be a value or a function that receives the sexp.

Example:
  (sly-test-with-mock-eval
      (((slynk:connection-info) . (:pid 1234 :version \"1.0\"))
       ((slynk:simple-completions \"foo\" \"CL-USER\")
        . ((\"foobar\" \"foobaz\") \"foo\")))
    (sly-eval \\='(slynk:connection-info)))"
  (declare (indent 1) (debug t))
  `(let ((sly-test-mock--responses ',responses)
         (sly-test-mock--call-log nil))
     (cl-letf (((symbol-function 'sly-eval) #'sly-test-mock-eval)
               ((symbol-function 'sly-eval-async) #'sly-test-mock-eval-async)
               ((symbol-function 'sly-connected-p) #'sly-test-mock-connected-p)
               ((symbol-function 'sly-current-connection) #'sly-test-mock-current-connection)
               ((symbol-function 'sly-current-package) #'sly-test-mock-current-package))
       (let ((sly-buffer-package "CL-USER")
             (sly-buffer-connection 'mock-connection))
         ,@body))))

(defmacro sly-test-with-mock-connection (&rest body)
  "Execute BODY with a mock connection available but no eval responses.
Use this for testing code that only checks connection status."
  (declare (indent 0) (debug t))
  `(sly-test-with-mock-eval nil ,@body))

;;; Assertion Helpers for Mocks
;;;
(defun sly-test-mock-assert-called (fn &optional times)
  "Assert that FN was called, optionally exactly TIMES times."
  (let ((calls (sly-test-mock--calls-to fn)))
    (if times
        (should (= (length calls) times))
      (should calls))))

(defun sly-test-mock-assert-called-with (fn expected-args)
  "Assert that FN was called with EXPECTED-ARGS at least once."
  (let ((calls (sly-test-mock--calls-to fn)))
    (should (cl-find expected-args calls
                     :test #'equal
                     :key #'cdr))))

(provide 'sly-test-mocks)
;;; sly-test-mocks.el ends here
