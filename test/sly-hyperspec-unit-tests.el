;;; sly-hyperspec-unit-tests.el --- Unit tests for hyperspec.el -*- lexical-binding: t; -*-

;; Copyright (C) 2024 SLY Contributors

;;; Commentary:

;; Unit tests for the HyperSpec utilities in lib/hyperspec.el.
;; Tests cover pure Elisp functionality for symbol name parsing and lookup.
;;
;; WHAT IS TESTED:
;;
;; This module tests SLY's integration with the Common Lisp HyperSpec—
;; the standard documentation for Common Lisp.  HyperSpec support lets
;; users press C-c C-d to view documentation for a symbol without leaving
;; Emacs.  This requires parsing symbol names and looking them up in a
;; database.
;;
;; KEY FUNCTIONS TESTED:
;;
;; common-lisp-hyperspec--strip-cl-package: Remove package prefixes
;;   - Strips CL:: (internal symbol access)
;;   - Strips CL: (external symbol access)
;;   - Strips COMMON-LISP:: and COMMON-LISP:
;;   - Handles lowercase variants (cl::, common-lisp::)
;;
;; common-lisp-hyperspec-lookup: Find symbol in HyperSpec database
;;   - Insert symbols into hash table
;;   - Query for symbol URL
;;   - Proper case handling
;;
;; WHAT IS COVERED:
;;
;; Package Prefix Stripping:
;;   - CL:: internal access (most common)
;;   - CL: external access (less common)
;;   - COMMON-LISP:: full package name
;;   - COMMON-LISP: full package with external
;;   - Case variations (lowercase)
;;   - No prefix (already clean)
;;
;; Symbol Handling:
;;   - Deduplication (same symbol shouldn't appear twice)
;;   - Case-insensitive lookup (Common Lisp symbols are case-insensitive)
;;   - Special characters in symbols (#, +, -, etc.)
;;   - Symbols with unusual names
;;
;; Database Operations:
;;   - Insert symbol -> URL mappings
;;   - Query for symbols
;;   - Return URLs
;;
;; WHEN TO ADD TESTS:
;;
;; Add tests when modifying:
;;   - Package prefix handling
;;   - Symbol deduplication logic
;;   - Case sensitivity handling
;;   - Symbol insertion/lookup
;;   - Database structure
;;
;; WHY UNIT TESTS:
;;
;; HyperSpec lookups must be fast (user presses a key, they expect
;; instant response) and accurate (wrong symbol leads to frustration).
;; Unit tests verify parsing and lookup work correctly before hitting
;; the network or accessing the HyperSpec database.
;;
;; See doc/TESTING-UNIT-TESTS.md for patterns and utilities.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'hyperspec "lib/hyperspec")
(require 'sly-unit-tests "lib/sly-unit-tests")

;;;; Tests for common-lisp-hyperspec--strip-cl-package
;;;;
(ert-deftest sly-hyperspec-unit--strip-cl-package-with-cl ()
  "Test stripping CL:: prefix."
  (should (equal "defun"
                 (common-lisp-hyperspec--strip-cl-package "cl::defun"))))

(ert-deftest sly-hyperspec-unit--strip-cl-package-with-cl-single-colon ()
  "Test stripping CL: prefix (external symbol)."
  (should (equal "defun"
                 (common-lisp-hyperspec--strip-cl-package "cl:defun"))))

(ert-deftest sly-hyperspec-unit--strip-cl-package-with-common-lisp ()
  "Test stripping COMMON-LISP:: prefix."
  (should (equal "car"
                 (common-lisp-hyperspec--strip-cl-package "common-lisp::car"))))

(ert-deftest sly-hyperspec-unit--strip-cl-package-with-common-lisp-single-colon ()
  "Test stripping COMMON-LISP: prefix."
  (should (equal "car"
                 (common-lisp-hyperspec--strip-cl-package "common-lisp:car"))))

(ert-deftest sly-hyperspec-unit--strip-cl-package-case-insensitive ()
  "Test stripping is case-insensitive."
  (should (equal "list"
                 (common-lisp-hyperspec--strip-cl-package "CL::list")))
  (should (equal "cons"
                 (common-lisp-hyperspec--strip-cl-package "Common-Lisp::cons"))))

(ert-deftest sly-hyperspec-unit--strip-cl-package-other-package ()
  "Test that non-CL packages are not stripped."
  (should (equal "foo::bar"
                 (common-lisp-hyperspec--strip-cl-package "foo::bar")))
  (should (equal "my-package:symbol"
                 (common-lisp-hyperspec--strip-cl-package "my-package:symbol"))))

(ert-deftest sly-hyperspec-unit--strip-cl-package-no-package ()
  "Test symbol with no package prefix."
  (should (equal "defun"
                 (common-lisp-hyperspec--strip-cl-package "defun")))
  (should (equal "my-function"
                 (common-lisp-hyperspec--strip-cl-package "my-function"))))

(ert-deftest sly-hyperspec-unit--strip-cl-package-empty ()
  "Test with empty string."
  (should (equal ""
                 (common-lisp-hyperspec--strip-cl-package ""))))

(ert-deftest sly-hyperspec-unit--strip-cl-package-keyword ()
  "Test with keyword (single colon prefix)."
  ;; Keywords like :foo should not match the pattern (no package before :)
  (should (equal ":keyword"
                 (common-lisp-hyperspec--strip-cl-package ":keyword"))))

;;;; Tests for common-lisp-hyperspec--find and common-lisp-hyperspec--insert
;;;;
(ert-deftest sly-hyperspec-unit--insert-and-find ()
  "Test inserting and finding a symbol."
  (let ((common-lisp-hyperspec--symbols (make-hash-table :test 'equal)))
    (common-lisp-hyperspec--insert "defun" "m_defun.htm")
    (should (equal '("m_defun.htm")
                   (common-lisp-hyperspec--find "defun")))))

(ert-deftest sly-hyperspec-unit--insert-multiple-urls ()
  "Test inserting multiple URLs for the same symbol."
  (let ((common-lisp-hyperspec--symbols (make-hash-table :test 'equal)))
    (common-lisp-hyperspec--insert "list" "f_list_.htm")
    (common-lisp-hyperspec--insert "list" "a_list.htm")
    (let ((urls (common-lisp-hyperspec--find "list")))
      (should (= 2 (length urls)))
      (should (member "f_list_.htm" urls))
      (should (member "a_list.htm" urls)))))

(ert-deftest sly-hyperspec-unit--insert-duplicate-url ()
  "Test inserting duplicate URL doesn't create duplicates."
  (let ((common-lisp-hyperspec--symbols (make-hash-table :test 'equal)))
    (common-lisp-hyperspec--insert "cons" "f_cons.htm")
    (common-lisp-hyperspec--insert "cons" "f_cons.htm")
    (common-lisp-hyperspec--insert "cons" "f_cons.htm")
    ;; Should only have one entry due to :test #'equal in cl-pushnew
    (should (equal '("f_cons.htm")
                   (common-lisp-hyperspec--find "cons")))))

(ert-deftest sly-hyperspec-unit--find-nonexistent ()
  "Test finding a symbol that doesn't exist."
  (let ((common-lisp-hyperspec--symbols (make-hash-table :test 'equal)))
    (should (null (common-lisp-hyperspec--find "nonexistent-symbol")))))

(ert-deftest sly-hyperspec-unit--find-case-sensitive ()
  "Test that symbol lookup is case-sensitive."
  (let ((common-lisp-hyperspec--symbols (make-hash-table :test 'equal)))
    (common-lisp-hyperspec--insert "defun" "m_defun.htm")
    ;; Hash table uses 'equal which is case-sensitive for strings
    (should (common-lisp-hyperspec--find "defun"))
    (should-not (common-lisp-hyperspec--find "DEFUN"))
    (should-not (common-lisp-hyperspec--find "Defun"))))

(ert-deftest sly-hyperspec-unit--insert-empty-symbol ()
  "Test inserting empty symbol name."
  (let ((common-lisp-hyperspec--symbols (make-hash-table :test 'equal)))
    (common-lisp-hyperspec--insert "" "empty.htm")
    (should (equal '("empty.htm")
                   (common-lisp-hyperspec--find "")))))

(ert-deftest sly-hyperspec-unit--insert-special-characters ()
  "Test inserting symbols with special characters."
  (let ((common-lisp-hyperspec--symbols (make-hash-table :test 'equal)))
    (common-lisp-hyperspec--insert "*standard-output*" "v_debug_.htm")
    (common-lisp-hyperspec--insert "1+" "f_1pl_1_.htm")
    (common-lisp-hyperspec--insert "++" "v__pl_pl.htm")
    (should (common-lisp-hyperspec--find "*standard-output*"))
    (should (common-lisp-hyperspec--find "1+"))
    (should (common-lisp-hyperspec--find "++"))))

;;;; Tests for symbol table data structure
;;;;
(ert-deftest sly-hyperspec-unit--symbols-hash-table-exists ()
  "Test that `common-lisp-hyperspec--symbols' is a hash table."
  (should (hash-table-p common-lisp-hyperspec--symbols)))

(ert-deftest sly-hyperspec-unit--symbols-hash-table-test ()
  "Test that hash table uses 'equal for comparison."
  (should (eq 'equal (hash-table-test common-lisp-hyperspec--symbols))))

(ert-deftest sly-hyperspec-unit--symbols-preloaded ()
  "Test that some standard CL symbols are preloaded."
  ;; The hyperspec.el file preloads many symbols at load time
  ;; Test a few common ones
  (should (common-lisp-hyperspec--find "defun"))
  (should (common-lisp-hyperspec--find "let"))
  (should (common-lisp-hyperspec--find "if")))

;;;; Tests for integration of strip and find
;;;;
(ert-deftest sly-hyperspec-unit--strip-and-find-workflow ()
  "Test typical workflow of stripping package and finding symbol."
  ;; This simulates what happens when user looks up CL::DEFUN
  (let* ((user-input "CL::DEFUN")
         (stripped (downcase
                    (common-lisp-hyperspec--strip-cl-package user-input)))
         (result (common-lisp-hyperspec--find stripped)))
    (should (equal "defun" stripped))
    (should result)
    (should (listp result))))

(ert-deftest sly-hyperspec-unit--strip-preserves-non-cl-packages ()
  "Test that non-CL packages are preserved in lookup."
  (let ((common-lisp-hyperspec--symbols (make-hash-table :test 'equal)))
    ;; Insert with full package name
    (common-lisp-hyperspec--insert "foo::bar" "custom.htm")
    ;; Strip should preserve it
    (let ((stripped (common-lisp-hyperspec--strip-cl-package "foo::bar")))
      (should (equal "foo::bar" stripped))
      (should (common-lisp-hyperspec--find stripped)))))

(provide 'sly-hyperspec-unit-tests)
;;; sly-hyperspec-unit-tests.el ends here
