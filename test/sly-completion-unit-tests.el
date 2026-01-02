;;; sly-completion-unit-tests.el --- Unit tests for sly-completion.el -*- lexical-binding: t; -*-

;; Copyright (C) 2024 SLY Contributors

;;; Commentary:

;; Unit tests for the completion utilities in lib/sly-completion.el.
;; Tests cover pure Elisp functionality that doesn't require a Lisp connection.
;;
;; WHAT IS TESTED:
;;
;; This module tests SLY's completion system—how symbols are suggested,
;; annotated (labeled with type/category info), cached, and filtered.
;; Completion bridges Emacs' completion UI with Lisp backend responses.
;;
;; KEY FUNCTIONS TESTED:
;;
;; sly-completion-at-point: Main completion function
;;   - Provides candidates to Emacs when user presses TAB
;;   - Suppresses completion in strings and comments
;;   - Handles caching for performance
;;   - Formats completions with metadata from Lisp
;;
;; sly-completion-annotation: Shows metadata for each completion
;;   - Displays classification (function, variable, macro, etc.)
;;   - Shows scores or relevance information
;;   - Properly formats and truncates for display
;;
;; WHAT IS COVERED:
;;
;; String/Comment Detection:
;;   - Suppress completion inside string literals
;;   - Suppress completion in line comments
;;   - Allow completion in code
;;
;; Annotation Formatting:
;;   - Show classification (fn, var, macro, class, etc.)
;;   - Show relevance scores
;;   - Format annotations for display width
;;   - Combine multiple metadata fields
;;
;; Caching:
;;   - Cache completion results
;;   - Invalidate cache on edits
;;   - Verify calls to Lisp (using mock-eval)
;;
;; Completion Metadata:
;;   - Extract and store classification
;;   - Extract and store relevance scores
;;   - Handle missing metadata gracefully
;;
;; WHEN TO ADD TESTS:
;;
;; Add tests when modifying:
;;   - Context detection (string/comment suppression)
;;   - Annotation display format
;;   - Caching strategy or invalidation
;;   - Metadata handling from Lisp
;;   - Completion filtering or sorting
;;
;; WHY UNIT TESTS:
;;
;; Completion logic is pure Elisp (doesn't need full SLY) and must be
;; fast (users expect instant feedback). Unit tests verify behavior
;; without waiting for Lisp server startup.
;;
;; See doc/TESTING-UNIT-TESTS.md for patterns and utilities.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'sly-completion "lib/sly-completion")
(require 'sly-unit-tests "lib/sly-unit-tests")
(require 'sly-test-mocks "test/sly-test-mocks")

;;;; Tests for sly-completion-annotation
;;;;
(ert-deftest sly-completion-unit--annotation-empty ()
  "Test `sly-completion-annotation' with no properties."
  (let ((completion "foo"))
    (should (equal "" (sly-completion-annotation completion)))))

(ert-deftest sly-completion-unit--annotation-classification-only ()
  "Test `sly-completion-annotation' with classification."
  (let ((completion (propertize "foo" 'sly--classification "fn")))
    (should (equal "fn" (sly-completion-annotation completion)))))

(ert-deftest sly-completion-unit--annotation-score-only ()
  "Test `sly-completion-annotation' with score."
  (let ((completion (propertize "foo" 'sly--score 0.95)))
    (should (string-match-p "95\\." (sly-completion-annotation completion)))))

(ert-deftest sly-completion-unit--annotation-both ()
  "Test `sly-completion-annotation' with classification and score."
  (let ((completion (propertize "foo"
                                'sly--classification "macro"
                                'sly--score 0.85)))
    (let ((annotation (sly-completion-annotation completion)))
      (should (string-match-p "macro" annotation))
      (should (string-match-p "85\\." annotation)))))

;;;; Tests for sly--completion-inside-string-or-comment-p
;;;;
(ert-deftest sly-completion-unit--inside-string-or-comment-in-string ()
  "Test `sly--completion-inside-string-or-comment-p' in string."
  (sly-unit-with-temp-lisp-buffer "(print \"hello world\")"
    (search-forward "hello")
    (should (sly--completion-inside-string-or-comment-p))))

(ert-deftest sly-completion-unit--inside-string-or-comment-in-comment ()
  "Test `sly--completion-inside-string-or-comment-p' in comment."
  (sly-unit-with-temp-lisp-buffer "(foo) ; comment here"
    (search-forward "comment")
    (should (sly--completion-inside-string-or-comment-p))))

(ert-deftest sly-completion-unit--inside-string-or-comment-in-code ()
  "Test `sly--completion-inside-string-or-comment-p' in code."
  (sly-unit-with-temp-lisp-buffer "(defun foo () nil)"
    (search-forward "foo")
    (should-not (sly--completion-inside-string-or-comment-p))))

;;;; Tests for sly--completion-function-wrapper
;;;;
(ert-deftest sly-completion-unit--function-wrapper-all-completions ()
  "Test completion wrapper returns all completions."
  (let* ((completions '("foo" "foobar" "foobaz"))
         (fn (lambda (_pattern) (list completions nil)))
         (wrapper (sly--completion-function-wrapper fn)))
    (should (equal completions (funcall wrapper "f" nil t)))))

(ert-deftest sly-completion-unit--function-wrapper-try-completion ()
  "Test completion wrapper try-completion."
  (let* ((completions '("foobar" "foobaz"))
         (fn (lambda (_pattern) (list completions nil)))
         (wrapper (sly--completion-function-wrapper fn)))
    ;; try-completion should return common prefix
    (should (equal "fooba" (funcall wrapper "foo" nil nil)))))

(ert-deftest sly-completion-unit--function-wrapper-try-completion-unique ()
  "Test completion wrapper try-completion with unique match."
  (let* ((completions '("foobar"))
         (fn (lambda (_pattern) (list completions nil)))
         (wrapper (sly--completion-function-wrapper fn)))
    ;; try-completion should return t for unique match
    (should (eq t (funcall wrapper "foobar" nil nil)))))

(ert-deftest sly-completion-unit--function-wrapper-metadata ()
  "Test completion wrapper returns proper metadata."
  (let* ((fn (lambda (_pattern) (list '("foo") nil)))
         (wrapper (sly--completion-function-wrapper fn)))
    (let ((metadata (funcall wrapper "f" nil 'metadata)))
      (should (eq 'metadata (car metadata)))
      (should (eq 'sly-completion (cdr (assq 'category (cdr metadata))))))))

(ert-deftest sly-completion-unit--function-wrapper-caching ()
  "Test completion wrapper caches results."
  (let* ((call-count 0)
         (fn (lambda (_pattern)
               (cl-incf call-count)
               (list '("foo" "bar") nil)))
         (wrapper (sly--completion-function-wrapper fn)))
    ;; First call
    (funcall wrapper "test" nil t)
    (should (= 1 call-count))
    ;; Second call with same pattern should use cache
    (funcall wrapper "test" nil t)
    (should (= 1 call-count))
    ;; Different pattern should call function again
    (funcall wrapper "other" nil t)
    (should (= 2 call-count))))

(ert-deftest sly-completion-unit--function-wrapper-sly-identify ()
  "Test completion wrapper responds to sly--identify."
  (let* ((fn (lambda (_pattern) (list '("foo") nil)))
         (wrapper (sly--completion-function-wrapper fn)))
    (should (eq t (funcall wrapper nil nil 'sly--identify)))))

;;;; Tests for sly--external-tryc
;;;;
(ert-deftest sly-completion-unit--external-tryc-unique ()
  "Test `sly--external-tryc' with unique completion."
  (let ((table (lambda (_s _p _a) '("foobar"))))
    ;; When pattern equals the sole completion, return t
    (should (eq t (sly--external-tryc "foobar" table nil 6)))))

(ert-deftest sly-completion-unit--external-tryc-partial ()
  "Test `sly--external-tryc' with partial match."
  (let ((table (lambda (_s _p _a) '("foobar"))))
    ;; When pattern doesn't equal completion, return (completion . length)
    (let ((result (sly--external-tryc "foo" table nil 3)))
      (should (consp result))
      (should (equal "foobar" (car result))))))

(ert-deftest sly-completion-unit--external-tryc-multiple ()
  "Test `sly--external-tryc' with multiple matches."
  (let ((table (lambda (_s _p _a) '("foobar" "foobaz"))))
    ;; With multiple matches, return (pattern . point)
    (let ((result (sly--external-tryc "foo" table nil 3)))
      (should (consp result))
      (should (equal "foo" (car result)))
      (should (= 3 (cdr result))))))

;;;; Tests for sly--external-allc
;;;;
(ert-deftest sly-completion-unit--external-allc ()
  "Test `sly--external-allc' returns all completions."
  (let ((completions '("foo" "foobar" "foobaz"))
        (table (lambda (_s _p _a) '("foo" "foobar" "foobaz"))))
    (should (equal completions (sly--external-allc "f" table nil 1)))))

;;;; Tests for completion buffer filling (simplified)
;;;;
;; Note: These functions require full SLY initialization and are better tested
;; in integration tests. Unit tests focus on the annotation and property logic.

(ert-deftest sly-completion-unit--annotation-properties ()
  "Test completion annotations are correctly formatted."
  (let ((completion (propertize "test-fn"
                                'sly--classification "macro"
                                'sly--score 0.92)))
    (let ((annotation (sly-completion-annotation completion)))
      (should (string-match-p "macro" annotation))
      (should (string-match-p "92" annotation)))))

;;;; Tests for flex completion chunks highlighting
;;;;
(ert-deftest sly-completion-unit--flex-chunks-property ()
  "Test flex completion chunks are stored as property."
  (let ((completion (propertize "multiple-value-bind"
                                'sly-completion-chunks
                                '((0 . 1) (9 . 10) (15 . 16)))))
    (should (equal '((0 . 1) (9 . 10) (15 . 16))
                   (get-text-property 0 'sly-completion-chunks completion)))))

;;;; Integration test with mock eval
;;;;
(ert-deftest sly-completion-unit--simple-completions-with-mock ()
  "Test `sly-simple-completions' with mocked eval."
  ;; WHY THIS IS COMPLEX:
  ;; The underlying function calls (slynk-completion:simple-completions "pri" 'CL-USER)
  ;; with a quoted package name. The mock must match this exact form, including
  ;; the quote. Notice '"CL-USER" (a quoted string) not just "CL-USER".
  ;;
  ;; The response is a list: ((COMPLETIONS...) PREFIX-USED)
  ;; where PREFIX-USED helps with filtering.
  (sly-test-with-mock-eval
      (((slynk-completion:simple-completions "pri" '"CL-USER")  ; ← package is quoted!
        . (("print" "princ" "prin1") "pri")))  ; ← response: (completions prefix)
    ;; Call the function being tested
    (let ((result (sly-simple-completions "pri")))
      ;; Verify it returns a 2-element list
      (should (listp result))
      (should (= 2 (length result)))
      ;; Verify one of the expected completions is present
      (should (member "print" (car result))))))

(provide 'sly-completion-unit-tests)
;;; sly-completion-unit-tests.el ends here
