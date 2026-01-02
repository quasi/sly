;;; sly-unit-tests.el --- Unit test framework for SLY -*- lexical-binding: t; -*-

;; Copyright (C) 2025 SLY Contributors

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Unit test utilities for testing SLY's pure Elisp code without
;; requiring a live Lisp connection.  These tests complement the
;; integration tests in lib/sly-tests.el.
;;
;; WHAT THIS FILE PROVIDES:
;;
;; This module contains reusable macros and functions for writing fast,
;; isolated unit tests that verify SLY's Elisp code without needing a
;; running Lisp server.  Tests can run in seconds and are completely
;; independent of external state.
;;
;; KEY UTILITIES:
;;
;; Buffer Creation:
;;   - sly-unit-with-temp-lisp-buffer: Create temp buffer with Lisp code
;;   - sly-unit-with-temp-lisp-buffer-point: Temp buffer with | marking point
;;
;; Function Mocking:
;;   - sly-unit-with-mock: Temporarily replace functions
;;   - sly-unit-with-stubbed-connection: Stub connection functions
;;
;; Overlay/Button Testing:
;;   - sly-unit-with-overlays: Create and clean up test overlays
;;   - sly-unit-make-test-overlay: Low-level overlay creation
;;   - sly-unit-propertize-region: Add text properties
;;
;; Assertions:
;;   - sly-unit-should-match: Assert string matches regex
;;   - sly-unit-should-equal-normalized: Assert equal after whitespace normalization
;;
;; Completion Testing:
;;   - sly-unit-make-completion-table: Create mock completion function
;;
;; USAGE PATTERN:
;;
;; (ert-deftest sly-example-unit--my-function-works ()
;;   "Test that my-function produces expected output."
;;   (sly-unit-with-temp-lisp-buffer "(defun foo () 42)"
;;     (goto-char 10)
;;     (should (equal "foo" (symbol-at-point)))))
;;
;; For more examples and patterns, see doc/TESTING-UNIT-TESTS.md.

;;; Code:

(require 'ert)
(require 'cl-lib)

;;; Test Buffer Utilities
;;;
(defmacro sly-unit-with-temp-lisp-buffer (content &rest body)
  "Execute BODY in a temp buffer with CONTENT in `lisp-mode'.
Point is placed at the beginning of the buffer."
  (declare (indent 1) (debug t))
  `(with-temp-buffer
     (lisp-mode)
     (insert ,content)
     (goto-char (point-min))
     ,@body))

(defmacro sly-unit-with-temp-lisp-buffer-point (content &rest body)
  "Like `sly-unit-with-temp-lisp-buffer' but | marks point position.
The | character is removed and point is placed there."
  (declare (indent 1) (debug t))
  `(with-temp-buffer
     (lisp-mode)
     (insert ,content)
     (goto-char (point-min))
     (when (search-forward "|" nil t)
       (delete-char -1))
     ,@body))

;;; Mock Function Utilities
;;;
(defmacro sly-unit-with-mock (bindings &rest body)
  "Execute BODY with functions temporarily rebound.
BINDINGS is a list of (FUNC REPLACEMENT) pairs.
Example: (sly-unit-with-mock ((foo (lambda () 42))) (foo)) => 42"
  (declare (indent 1) (debug t))
  (let ((originals (cl-gensym "originals")))
    `(let ((,originals
            (list ,@(mapcar (lambda (b)
                              `(cons ',(car b)
                                     (when (fboundp ',(car b))
                                       (symbol-function ',(car b)))))
                            bindings))))
       (unwind-protect
           (progn
             ,@(mapcar (lambda (b)
                         `(fset ',(car b) ,(cadr b)))
                       bindings)
             ,@body)
         (dolist (orig ,originals)
           (if (cdr orig)
               (fset (car orig) (cdr orig))
             (fmakunbound (car orig))))))))

(defmacro sly-unit-with-stubbed-connection (&rest body)
  "Execute BODY with SLY connection functions stubbed.
This allows testing code that checks for connection without
actually requiring one."
  (declare (indent 0) (debug t))
  `(sly-unit-with-mock
       ((sly-connected-p (lambda () t))
        (sly-current-connection (lambda () 'mock-connection))
        (sly-current-package (lambda () "CL-USER"))
        (sly-buffer-package (lambda () "CL-USER")))
     (let ((sly-buffer-package "CL-USER")
           (sly-buffer-connection 'mock-connection))
       ,@body)))

;;; Assertion Helpers
;;;
(defun sly-unit-should-match (pattern string)
  "Assert that STRING matches regexp PATTERN."
  (should (string-match-p pattern string)))

(defun sly-unit-should-equal-normalized (expected actual)
  "Assert EXPECTED equals ACTUAL after normalizing whitespace."
  (let ((normalize (lambda (s)
                     (replace-regexp-in-string "[ \t\n]+" " "
                                               (string-trim s)))))
    (should (equal (funcall normalize expected)
                   (funcall normalize actual)))))

;;; Test Registration Helpers
;;;
(defmacro sly-unit-deftest (name args doc &rest body)
  "Define a unit test NAME with ARGS, DOC, and BODY.
This is a thin wrapper around `ert-deftest' that adds the 'sly-unit tag."
  (declare (indent 2) (debug t))
  `(ert-deftest ,(intern (format "sly-unit--%s" name)) ()
     ,doc
     :tags '(sly sly-unit)
     ,@body))

;;; Overlay/Button Test Utilities
;;;
(defun sly-unit-make-test-overlay (beg end &rest props)
  "Create a test overlay from BEG to END with PROPS."
  (let ((ov (make-overlay beg end)))
    (while props
      (overlay-put ov (pop props) (pop props)))
    ov))

(defmacro sly-unit-with-overlays (specs &rest body)
  "Execute BODY with overlays created per SPECS.
SPECS is a list of (VAR BEG END PROPS...) forms.
Overlays are deleted after BODY."
  (declare (indent 1) (debug t))
  (let ((ovs (mapcar (lambda (spec)
                       (let ((var (car spec))
                             (beg (cadr spec))
                             (end (caddr spec))
                             (props (cdddr spec)))
                         `(,var (sly-unit-make-test-overlay ,beg ,end ,@props))))
                     specs)))
    `(let ,ovs
       (unwind-protect
           (progn ,@body)
         ,@(mapcar (lambda (spec) `(delete-overlay ,(car spec))) specs)))))

;;; Text Property Test Utilities
;;;
(defun sly-unit-propertize-region (beg end &rest props)
  "Add PROPS to region from BEG to END."
  (add-text-properties beg end props))

(defun sly-unit-has-property-p (pos prop &optional value)
  "Check if POS has text property PROP, optionally with VALUE."
  (let ((actual (get-text-property pos prop)))
    (if value
        (equal actual value)
      actual)))

;;; Completion Test Utilities
;;;
(defun sly-unit-make-completion-table (completions)
  "Create a completion table function returning COMPLETIONS.
COMPLETIONS should be a list of strings."
  (lambda (string pred action)
    (pcase action
      ('metadata '(metadata (category . sly-completion)))
      ('t completions)
      ('nil (try-completion string completions pred))
      (`(boundaries . ,suffix)
       (completion-boundaries string completions pred suffix))
      (_ nil))))

(provide 'sly-unit-tests)
;;; sly-unit-tests.el ends here
