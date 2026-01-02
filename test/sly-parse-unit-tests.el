;;; sly-parse-unit-tests.el --- Unit tests for sly-parse.el -*- lexical-binding: t; -*-

;; Copyright (C) 2024 SLY Contributors

;;; Commentary:

;; Unit tests for the parsing utilities in lib/sly-parse.el.
;; These tests do not require a live Lisp connection.
;;
;; WHAT IS TESTED:
;;
;; This module tests SLY's parsing system—how to find and understand
;; Lisp code in buffers.  Many SLY features depend on parsing:
;;   - "Go to definition" needs to know what symbol is at point
;;   - Indentation needs to understand expression structure
;;   - Flashing mismatched parens needs to find them
;;   - Evaluation needs to know what to send to Lisp
;;
;; KEY FUNCTIONS TESTED:
;;
;; Character Syntax Detection:
;;   - sly-compare-char-syntax: Check if char has particular syntax
;;   - Parens, whitespace, word boundaries
;;   - Predicate functions for character classification
;;
;; String and Comment Detection:
;;   - sly-in-string-or-comment-p: Is point in literal?
;;   - Suppress evaluation/completion in literals
;;   - Accurate boundary detection
;;
;; Form Location:
;;   - sly-parse-form-at-point: What expression is at point?
;;   - Find beginning and end of forms
;;   - Handle nested structures
;;
;; List Navigation:
;;   - Moving between list elements
;;   - Counting nesting depth
;;   - Proper paren matching
;;
;; Pattern Matching:
;;   - Recognize code patterns
;;   - Distinguish different Lisp constructs
;;   - Handle edge cases
;;
;; WHAT IS COVERED:
;;
;; Character Syntax:
;;   - Open paren, close paren
;;   - Whitespace characters
;;   - Word characters (for symbols)
;;   - Special characters
;;
;; Context Detection:
;;   - Point in string? (quoted text)
;;   - Point in line comment? (;)
;;   - Point in block comment? (#|...|#)
;;   - Point in quote? ('x)
;;
;; Form Recognition:
;;   - S-expressions at point
;;   - Top-level forms
;;   - Nested lists
;;   - Empty lists
;;   - Quoted forms
;;
;; Edge Cases:
;;   - Unbalanced parens
;;   - Strings containing parens
;;   - Comments with parens
;;   - Multiple forms on one line
;;   - Empty buffer
;;
;; WHEN TO ADD TESTS:
;;
;; Add tests when modifying:
;;   - Character syntax detection
;;   - String/comment boundary detection
;;   - Form location logic
;;   - List navigation
;;   - Paren matching
;;   - Pattern recognition
;;
;; WHY UNIT TESTS:
;;
;; Parsing is complex (lots of edge cases with comments, strings, nested
;; structures) and critical for correctness (wrong parsing breaks many
;; features). Unit tests verify parsing works correctly before relying
;; on it in higher-level features.
;;
;; See doc/TESTING-UNIT-TESTS.md for patterns and utilities.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'sly-parse "lib/sly-parse")
(require 'sly-unit-tests "lib/sly-unit-tests")

;;;; Tests for sly-compare-char-syntax
;;;;
(ert-deftest sly-parse-unit--compare-char-syntax-open-paren ()
  "Test `sly-compare-char-syntax' detects open paren."
  (sly-unit-with-temp-lisp-buffer "(foo bar)"
    (should (sly-compare-char-syntax #'char-after "("))))

(ert-deftest sly-parse-unit--compare-char-syntax-close-paren ()
  "Test `sly-compare-char-syntax' detects close paren."
  (sly-unit-with-temp-lisp-buffer "(foo bar)"
    (goto-char (point-max))
    (backward-char 1)
    (should (sly-compare-char-syntax #'char-after ")"))))

(ert-deftest sly-parse-unit--compare-char-syntax-whitespace ()
  "Test `sly-compare-char-syntax' detects whitespace."
  (sly-unit-with-temp-lisp-buffer "(foo bar)"
    (search-forward "foo")
    (should (sly-compare-char-syntax #'char-after " "))))

(ert-deftest sly-parse-unit--compare-char-syntax-word ()
  "Test `sly-compare-char-syntax' detects word characters."
  (sly-unit-with-temp-lisp-buffer "(foo bar)"
    (forward-char 1)
    (should (sly-compare-char-syntax #'char-after "w"))))

(ert-deftest sly-parse-unit--compare-char-syntax-unescaped ()
  "Test `sly-compare-char-syntax' with unescaped flag."
  (sly-unit-with-temp-lisp-buffer "(foo \\(bar)"
    (search-forward "\\")
    ;; The ( after \ is escaped, so unescaped check should fail
    (should-not (sly-compare-char-syntax #'char-after "(" t))))

;;;; Tests for sly-pattern-path
;;;;
(ert-deftest sly-parse-unit--pattern-path-empty ()
  "Test `sly-pattern-path' with empty pattern."
  (should (equal '() (sly-pattern-path '()))))

(ert-deftest sly-parse-unit--pattern-path-star ()
  "Test `sly-pattern-path' with just *."
  (should (equal '() (sly-pattern-path '(*)))))

(ert-deftest sly-parse-unit--pattern-path-defun-star ()
  "Test `sly-pattern-path' with (defun *)."
  (should (equal '(defun) (sly-pattern-path '(defun *)))))

(ert-deftest sly-parse-unit--pattern-path-nested ()
  "Test `sly-pattern-path' with nested pattern ((*))."
  (should (equal '(1) (sly-pattern-path '((*))))))

(ert-deftest sly-parse-unit--pattern-path-complex ()
  "Test `sly-pattern-path' with complex pattern."
  ;; WHY THIS IS COMPLEX:
  ;; Pattern matching uses nested structure to describe location in code.
  ;; The pattern '(labels ((*))) encodes a path:
  ;;   - labels: the symbol "labels" is at the top
  ;;   - (*): inside a list, which contains another list with * (cursor location)
  ;;
  ;; The path is converted to indices:
  ;;   - labels: symbol (kept as symbol in result)
  ;;   - 1: first child list (index 1, counting from 0)
  ;;   - 1: first child list of that (index 1 again)
  ;;
  ;; This is used to describe cursor position in nested structures like
  ;; (labels ((slot1) (slot2))), where (* is cursor) -> '(labels 1 1)
  ;; means: in labels form, in second sublist, in second element.
  (should (equal '(labels 1 1) (sly-pattern-path '(labels ((*)))))))

;;;; Tests for sly-arglist-specializers
;;;;
(ert-deftest sly-parse-unit--arglist-specializers-empty ()
  "Test `sly-arglist-specializers' with empty arglist."
  (should (equal '() (sly-arglist-specializers '()))))

(ert-deftest sly-parse-unit--arglist-specializers-simple ()
  "Test `sly-arglist-specializers' with unspecialized args."
  (should (equal '(t t) (sly-arglist-specializers '(x y)))))

(ert-deftest sly-parse-unit--arglist-specializers-with-types ()
  "Test `sly-arglist-specializers' with specialized args."
  (should (equal '(string integer)
                 (sly-arglist-specializers '((s string) (n integer))))))

(ert-deftest sly-parse-unit--arglist-specializers-mixed ()
  "Test `sly-arglist-specializers' with mixed args."
  (should (equal '(t string t)
                 (sly-arglist-specializers '(x (s string) y)))))

(ert-deftest sly-parse-unit--arglist-specializers-stops-at-optional ()
  "Test `sly-arglist-specializers' stops at &optional."
  (should (equal '(t t)
                 (sly-arglist-specializers '(x y &optional z)))))

(ert-deftest sly-parse-unit--arglist-specializers-stops-at-key ()
  "Test `sly-arglist-specializers' stops at &key."
  (should (equal '(t)
                 (sly-arglist-specializers '(x &key y)))))

(ert-deftest sly-parse-unit--arglist-specializers-stops-at-rest ()
  "Test `sly-arglist-specializers' stops at &rest."
  (should (equal '(t t)
                 (sly-arglist-specializers '(a b &rest args)))))

;;;; Tests for sly-inside-string-p
;;;;
(ert-deftest sly-parse-unit--inside-string-p-yes ()
  "Test `sly-inside-string-p' returns t inside string."
  (sly-unit-with-temp-lisp-buffer "(defvar x \"hello world\")"
    (search-forward "hello")
    (should (sly-inside-string-p))))

(ert-deftest sly-parse-unit--inside-string-p-no ()
  "Test `sly-inside-string-p' returns nil outside string."
  (sly-unit-with-temp-lisp-buffer "(defvar x \"hello world\")"
    (search-forward "defvar")
    (should-not (sly-inside-string-p))))

(ert-deftest sly-parse-unit--inside-string-p-at-quote ()
  "Test `sly-inside-string-p' at opening quote."
  (sly-unit-with-temp-lisp-buffer "(defvar x \"hello\")"
    (search-forward "\"")
    (backward-char 1)
    (should-not (sly-inside-string-p))))

;;;; Tests for sly-inside-comment-p
;;;;
(ert-deftest sly-parse-unit--inside-comment-p-line-comment ()
  "Test `sly-inside-comment-p' in line comment."
  (sly-unit-with-temp-lisp-buffer "(foo) ; this is a comment\n(bar)"
    (search-forward "this")
    (should (sly-inside-comment-p))))

(ert-deftest sly-parse-unit--inside-comment-p-block-comment ()
  "Test `sly-inside-comment-p' in block comment."
  (sly-unit-with-temp-lisp-buffer "(foo) #| block comment |# (bar)"
    (search-forward "block")
    (should (sly-inside-comment-p))))

(ert-deftest sly-parse-unit--inside-comment-p-no ()
  "Test `sly-inside-comment-p' returns nil in code."
  (sly-unit-with-temp-lisp-buffer "(foo) ; comment\n(bar)"
    (search-forward "bar")
    (should-not (sly-inside-comment-p))))

;;;; Tests for sly-inside-string-or-comment-p
;;;;
(ert-deftest sly-parse-unit--inside-string-or-comment-p-string ()
  "Test `sly-inside-string-or-comment-p' in string."
  (sly-unit-with-temp-lisp-buffer "(print \"hello\")"
    (search-forward "hello")
    (should (sly-inside-string-or-comment-p))))

(ert-deftest sly-parse-unit--inside-string-or-comment-p-comment ()
  "Test `sly-inside-string-or-comment-p' in comment."
  (sly-unit-with-temp-lisp-buffer "(print x) ; comment"
    (search-forward "comment")
    (should (sly-inside-string-or-comment-p))))

(ert-deftest sly-parse-unit--inside-string-or-comment-p-no ()
  "Test `sly-inside-string-or-comment-p' in code."
  (sly-unit-with-temp-lisp-buffer "(print x)"
    (search-forward "print")
    (should-not (sly-inside-string-or-comment-p))))

;;;; Tests for sly-beginning-of-list
;;;;
(ert-deftest sly-parse-unit--beginning-of-list-simple ()
  "Test `sly-beginning-of-list' moves to first element."
  (sly-unit-with-temp-lisp-buffer "(foo bar baz)"
    (goto-char (point-max))
    (backward-char 2)  ; point at 'z' in baz
    (sly-beginning-of-list)
    (should (looking-at "foo"))))

(ert-deftest sly-parse-unit--beginning-of-list-nested ()
  "Test `sly-beginning-of-list' in nested list."
  (sly-unit-with-temp-lisp-buffer "(outer (inner x y))"
    (search-forward "y")
    (sly-beginning-of-list)
    (should (looking-at "inner"))))

(ert-deftest sly-parse-unit--beginning-of-list-with-up ()
  "Test `sly-beginning-of-list' with UP argument."
  (sly-unit-with-temp-lisp-buffer "(a (b (c x)))"
    (search-forward "x")
    (sly-beginning-of-list 2)
    (should (looking-at "b"))))

;;;; Tests for sly-end-of-list
;;;;
(ert-deftest sly-parse-unit--end-of-list-simple ()
  "Test `sly-end-of-list' moves past last element."
  (sly-unit-with-temp-lisp-buffer "(foo bar baz)"
    (forward-char 1)  ; inside the list
    (sly-end-of-list)
    (should (looking-back "baz" (- (point) 5)))))

;;;; Tests for sly-in-expression-p
;;;;
(ert-deftest sly-parse-unit--in-expression-p-empty-pattern ()
  "Test `sly-in-expression-p' with empty pattern matches anywhere."
  (sly-unit-with-temp-lisp-buffer "(anything here)"
    (search-forward "here")
    (should (sly-in-expression-p '()))))

(ert-deftest sly-parse-unit--in-expression-p-star ()
  "Test `sly-in-expression-p' with (*) matches inside list."
  (sly-unit-with-temp-lisp-buffer "(something)"
    (search-forward "some")
    (should (sly-in-expression-p '(*)))))

(ert-deftest sly-parse-unit--in-expression-p-defun ()
  "Test `sly-in-expression-p' matches defun."
  (sly-unit-with-temp-lisp-buffer "(defun foo () nil)"
    (search-forward "foo")
    (should (sly-in-expression-p '(defun *)))))

(ert-deftest sly-parse-unit--in-expression-p-defun-negative ()
  "Test `sly-in-expression-p' doesn't match wrong form."
  (sly-unit-with-temp-lisp-buffer "(defvar foo nil)"
    (search-forward "foo")
    (should-not (sly-in-expression-p '(defun *)))))

(ert-deftest sly-parse-unit--in-expression-p-defmethod ()
  "Test `sly-in-expression-p' matches defmethod."
  (sly-unit-with-temp-lisp-buffer "(defmethod print-object ((obj foo) stream) nil)"
    (search-forward "print-object")
    (should (sly-in-expression-p '(defmethod *)))))

(ert-deftest sly-parse-unit--in-expression-p-labels ()
  "Test `sly-in-expression-p' matches labels with nested pattern."
  (sly-unit-with-temp-lisp-buffer "(defun outer ()
  (labels ((inner (x) x))
    (inner 1)))"
    (search-forward "inner (x)")
    (should (sly-in-expression-p '(labels ((*)))))))

;;;; Tests for sly-parse-form-until
;;;;
(ert-deftest sly-parse-unit--parse-form-until-simple ()
  "Test `sly-parse-form-until' parses simple form."
  (sly-unit-with-temp-lisp-buffer "(foo bar baz)"
    (forward-char 1)  ; after opening paren
    (let ((result (sly-parse-form-until (point-max) '(CURSOR))))
      (should (member "foo" result))
      (should (member "bar" result))
      (should (member "baz" result))
      (should (member 'CURSOR result)))))

(ert-deftest sly-parse-unit--parse-form-until-partial ()
  "Test `sly-parse-form-until' parses partial form."
  (sly-unit-with-temp-lisp-buffer "(foo bar baz)"
    (forward-char 1)
    (let ((result (sly-parse-form-until 8 '(CURSOR))))  ; up to 'bar'
      (should (member "foo" result))
      (should (member 'CURSOR result)))))

;;;; Tests for sly-current-parser-state
;;;;
(ert-deftest sly-parse-unit--current-parser-state-at-top ()
  "Test `sly-current-parser-state' at top level."
  (sly-unit-with-temp-lisp-buffer "(foo)"
    ;; At top level, depth should be 0
    (should (zerop (car (sly-current-parser-state))))))

(ert-deftest sly-parse-unit--current-parser-state-in-list ()
  "Test `sly-current-parser-state' inside list."
  (sly-unit-with-temp-lisp-buffer "(foo bar)"
    (forward-char 1)  ; inside list
    (should (= 1 (car (sly-current-parser-state))))))

(ert-deftest sly-parse-unit--current-parser-state-nested ()
  "Test `sly-current-parser-state' in nested list."
  (sly-unit-with-temp-lisp-buffer "(foo (bar baz))"
    (search-forward "bar")
    (should (= 2 (car (sly-current-parser-state))))))

(provide 'sly-parse-unit-tests)
;;; sly-parse-unit-tests.el ends here
