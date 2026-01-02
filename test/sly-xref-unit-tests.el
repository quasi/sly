;;; sly-xref-unit-tests.el --- Unit tests for sly-xref.el -*- lexical-binding: t; -*-

;; Copyright (C) 2026 SLY Contributors

;;; Commentary:

;; Unit tests for the xref.el integration in lib/sly-xref.el.
;; Tests cover location conversion, backend methods, and query functionality.
;;
;; WHAT IS TESTED:
;;
;; This module tests SLY's integration with Emacs's xref.el framework.
;; The integration allows standard M-. / M-, navigation to work with
;; Common Lisp definitions via the Slynk server.
;;
;; KEY FUNCTIONS TESTED:
;;
;; Location Conversion:
;;   sly-xref--convert-to-xref-items: Convert Slynk results to xref objects
;;   sly-xref--convert-location: Convert single Slynk location
;;   sly-xref--make-location: Create xref location from buffer/position specs
;;   sly-xref--position-to-line-col: Byte position to line/column
;;   sly-xref--resolve-position-in-file: Handle different position types
;;
;; Backend Methods:
;;   xref-backend-identifier-at-point: Get symbol at point
;;   xref-backend-definitions: Find definitions
;;   xref-backend-references: Find references
;;   xref-backend-apropos: Pattern-based symbol search
;;
;; Query System:
;;   sly-xref-query: Unified query command for all xref types
;;   sly-xref-query-types: Registry of query type keywords
;;
;; WHAT IS COVERED:
;;
;; Location Conversion:
;;   - File locations with byte positions
;;   - File locations with line/column
;;   - Buffer locations
;;   - Error/bogus locations
;;   - Position types: :position, :offset, :line, :function-name
;;
;; Backend Protocol:
;;   - Backend registration via xref-backend-functions
;;   - Identifier extraction at point
;;   - Definition lookup with mock responses
;;   - Reference lookup with mock responses
;;   - Handling :not-implemented responses
;;
;; Query Types:
;;   - All supported types: :calls, :calls-who, :references, etc.
;;   - Error handling for unimplemented queries
;;   - Empty result handling
;;
;; WHEN TO ADD TESTS:
;;
;; Add tests when modifying:
;;   - Location conversion logic
;;   - Backend method implementations
;;   - Query type handling
;;   - Error cases and edge conditions
;;
;; WHY UNIT TESTS:
;;
;; The xref integration has many edge cases in location conversion
;; (different position types, file vs buffer, errors) that need testing
;; without a running Lisp server. These tests verify the conversion
;; logic is correct before integration testing.
;;
;; See doc/TESTING-UNIT-TESTS.md for patterns and utilities.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'xref)
(require 'sly-test-mocks "test/sly-test-mocks")

;; Forward declarations - actual implementation will be in lib/sly-xref.el
(declare-function sly-xref--convert-to-xref-items "lib/sly-xref")
(declare-function sly-xref--convert-location "lib/sly-xref")
(declare-function sly-xref--make-location "lib/sly-xref")
(declare-function sly-xref--position-to-line-col "lib/sly-xref")
(declare-function sly-xref--resolve-position-in-file "lib/sly-xref")
(declare-function sly-xref-backend "lib/sly-xref")
(declare-function sly-xref-query "lib/sly-xref")

;; Load the module under test when available
(when (locate-library "sly-xref" nil (list (expand-file-name "lib" (file-name-directory load-file-name)))))

;;;; Location Conversion Tests
;;;;

(ert-deftest sly-xref-unit--convert-file-location-with-position ()
  "Test conversion of Slynk file location with :position to xref."
  (skip-unless (fboundp 'sly-xref--convert-location))
  (let* ((slynk-loc '(:location (:file "/tmp/test.lisp")
                                (:position 100)
                                (:snippet "(defun foo")))
         (xref-loc (sly-xref--convert-location slynk-loc)))
    (should xref-loc)
    (should (xref-file-location-p xref-loc))))

(ert-deftest sly-xref-unit--convert-file-location-with-line ()
  "Test conversion of Slynk file location with :line to xref."
  (skip-unless (fboundp 'sly-xref--convert-location))
  (let* ((slynk-loc '(:location (:file "/tmp/test.lisp")
                                (:line 10 5)
                                nil))
         (xref-loc (sly-xref--convert-location slynk-loc)))
    (should xref-loc)
    (should (xref-file-location-p xref-loc))
    (should (= 10 (xref-file-location-line xref-loc)))
    ;; xref.el uses 0-based columns; input column 5 (1-based) = column 4 (0-based)
    (should (= 4 (xref-file-location-column xref-loc)))))

(ert-deftest sly-xref-unit--convert-buffer-location ()
  "Test conversion of Slynk buffer location to xref."
  (skip-unless (fboundp 'sly-xref--convert-location))
  (with-temp-buffer
    (rename-buffer "*test-buffer*" t)
    (insert "(defun foo () 42)")
    (let* ((buf-name (buffer-name))
           (slynk-loc `(:location (:buffer ,buf-name)
                                  (:position 8)
                                  nil))
           (xref-loc (sly-xref--convert-location slynk-loc)))
      (should xref-loc)
      (should (xref-buffer-location-p xref-loc)))))

(ert-deftest sly-xref-unit--convert-error-location ()
  "Test conversion of Slynk error location to bogus xref."
  (skip-unless (fboundp 'sly-xref--convert-location))
  (let* ((slynk-loc '(:error "Source not available"))
         (xref-loc (sly-xref--convert-location slynk-loc)))
    (should xref-loc)
    (should (xref-bogus-location-p xref-loc))
    (should (string-match "Source not available"
                          (xref-bogus-location-message xref-loc)))))

(ert-deftest sly-xref-unit--convert-source-form-location ()
  "Test conversion of :source-form location (no file) to bogus xref."
  (skip-unless (fboundp 'sly-xref--convert-location))
  (let* ((slynk-loc '(:location (:source-form "(defun foo () 42)")
                                (:position 1)
                                nil))
         (xref-loc (sly-xref--convert-location slynk-loc)))
    (should xref-loc)
    (should (xref-bogus-location-p xref-loc))))

(ert-deftest sly-xref-unit--convert-nil-location ()
  "Test that nil location returns nil."
  (skip-unless (fboundp 'sly-xref--convert-location))
  (should-not (sly-xref--convert-location nil)))

(ert-deftest sly-xref-unit--convert-multiple-xrefs ()
  "Test conversion of multiple Slynk xrefs to xref-items."
  (skip-unless (fboundp 'sly-xref--convert-to-xref-items))
  (let* ((slynk-xrefs
          '(("(defun foo)" (:location (:file "/tmp/a.lisp") (:line 10) nil))
            ("(defun bar)" (:location (:file "/tmp/b.lisp") (:line 20) nil))
            ("(defmethod foo)" (:error "Compiled code"))))
         (xref-items (sly-xref--convert-to-xref-items slynk-xrefs)))
    (should (= 3 (length xref-items)))
    (should (cl-every #'xref-item-p xref-items))
    ;; First two have real locations, third has bogus
    (should (xref-file-location-p (xref-item-location (nth 0 xref-items))))
    (should (xref-file-location-p (xref-item-location (nth 1 xref-items))))
    (should (xref-bogus-location-p (xref-item-location (nth 2 xref-items))))))

;;;; Position Resolution Tests
;;;;

(ert-deftest sly-xref-unit--position-to-line-col-basic ()
  "Test byte position to line/column conversion."
  (skip-unless (fboundp 'sly-xref--position-to-line-col))
  (let ((temp-file (make-temp-file "sly-xref-test" nil ".lisp")))
    (unwind-protect
        (progn
          (with-temp-file temp-file
            (insert "(defun foo ()\n")  ; line 1: 15 chars
            (insert "  42)\n"))          ; line 2: 6 chars
          (let ((line-col (sly-xref--position-to-line-col
                           temp-file '(:position 18))))
            ;; Position 18 is "4" on line 2
            (should (equal 2 (car line-col)))))
      (delete-file temp-file))))

(ert-deftest sly-xref-unit--position-to-line-col-line-spec ()
  "Test :line position spec passes through correctly."
  (skip-unless (fboundp 'sly-xref--position-to-line-col))
  (let ((temp-file (make-temp-file "sly-xref-test" nil ".lisp")))
    (unwind-protect
        (progn
          (with-temp-file temp-file
            (insert "line1\nline2\nline3\n"))
          (let ((line-col (sly-xref--position-to-line-col
                           temp-file '(:line 2 3))))
            (should (equal 2 (car line-col)))
            ;; xref.el uses 0-based columns; input column 3 (1-based) = column 2 (0-based)
            (should (equal 2 (cdr line-col)))))
      (delete-file temp-file))))

(ert-deftest sly-xref-unit--position-to-line-col-function-name ()
  "Test :function-name position spec finds function."
  (skip-unless (fboundp 'sly-xref--resolve-position-in-file))
  (let ((temp-file (make-temp-file "sly-xref-test" nil ".lisp")))
    (unwind-protect
        (progn
          (with-temp-file temp-file
            (insert "(defpackage :test)\n")
            (insert "(defun my-special-function ()\n")
            (insert "  42)\n"))
          (let ((pos (sly-xref--resolve-position-in-file
                      temp-file '(:function-name "my-special-function"))))
            ;; Should find the function name
            (should (> pos 20))))
      (delete-file temp-file))))

(ert-deftest sly-xref-unit--position-to-line-col-error-fallback ()
  "Test that position conversion falls back gracefully on errors."
  (skip-unless (fboundp 'sly-xref--position-to-line-col))
  ;; Non-existent file should return (1 . 0)
  (let ((line-col (sly-xref--position-to-line-col
                   "/nonexistent/file.lisp" '(:position 100))))
    (should (equal '(1 . 0) line-col))))

;;;; Backend Registration Tests
;;;;

(ert-deftest sly-xref-unit--backend-returns-sly-when-connected ()
  "Test that backend function returns 'sly when connected."
  (skip-unless (fboundp 'sly-xref-backend))
  (sly-test-with-mock-connection
    (should (eq 'sly (sly-xref-backend)))))

(ert-deftest sly-xref-unit--backend-returns-nil-when-disconnected ()
  "Test that backend function returns nil when not connected."
  (skip-unless (fboundp 'sly-xref-backend))
  ;; Without mock connection, sly-connected-p returns nil
  (cl-letf (((symbol-function 'sly-connected-p) (lambda () nil)))
    (should-not (sly-xref-backend))))

;;;; Backend Method Tests
;;;;

(ert-deftest sly-xref-unit--backend-definitions-basic ()
  "Test xref-backend-definitions with mock eval."
  (skip-unless (fboundp 'xref-backend-definitions))
  (sly-test-with-mock-eval
      (((slynk:find-definitions-for-emacs "foo")
        . (("(defun foo)" (:location (:file "/tmp/test.lisp") (:line 10) nil)))))
    (let ((xrefs (xref-backend-definitions 'sly "foo")))
      (should (listp xrefs))
      (should (= 1 (length xrefs)))
      (should (xref-item-p (car xrefs)))
      (should (string-match "defun foo" (xref-item-summary (car xrefs)))))))

(ert-deftest sly-xref-unit--backend-definitions-no-results ()
  "Test xref-backend-definitions with no results."
  (skip-unless (fboundp 'xref-backend-definitions))
  (sly-test-with-mock-eval
      (((slynk:find-definitions-for-emacs "nonexistent") . nil))
    (let ((xrefs (xref-backend-definitions 'sly "nonexistent")))
      (should (null xrefs)))))

(ert-deftest sly-xref-unit--backend-references-basic ()
  "Test xref-backend-references with mock eval."
  (skip-unless (fboundp 'xref-backend-references))
  (sly-test-with-mock-eval
      (((slynk:xref ':callers "foo")
        . (("(defun bar)" (:location (:file "/tmp/bar.lisp") (:line 5) nil)))))
    (let ((xrefs (xref-backend-references 'sly "foo")))
      (should (listp xrefs))
      (should (= 1 (length xrefs))))))

(ert-deftest sly-xref-unit--backend-references-not-implemented ()
  "Test xref-backend-references when Lisp returns :not-implemented."
  (skip-unless (fboundp 'xref-backend-references))
  (sly-test-with-mock-eval
      (((slynk:xref ':callers "foo") . :not-implemented))
    (let ((xrefs (xref-backend-references 'sly "foo")))
      (should (null xrefs)))))

(ert-deftest sly-xref-unit--backend-apropos-basic ()
  "Test xref-backend-apropos with mock eval."
  (skip-unless (fboundp 'xref-backend-apropos))
  (sly-test-with-mock-eval
      (((slynk:apropos-list-for-emacs "foo" nil nil)
        . (("FOO" 0 (:function :variable))
           ("FOOBAR" 0 (:function)))))
    (let ((xrefs (xref-backend-apropos 'sly "foo")))
      (should (listp xrefs))
      (should (= 2 (length xrefs)))
      ;; Apropos returns bogus locations (no source info)
      (should (xref-bogus-location-p (xref-item-location (car xrefs)))))))

;;;; Query System Tests
;;;;

(ert-deftest sly-xref-unit--query-calls ()
  "Test sly-xref-query with :calls type."
  (skip-unless (fboundp 'sly-xref-query))
  (sly-test-with-mock-eval
      ;; Note: sly-xref-query uses `(slynk:xref ',type ',symbol) which quotes args
      (((slynk:xref ':calls '"foo")
        . (("(defun bar)" (:location (:file "/tmp/bar.lisp") (:line 5) nil)))))
    ;; Just verify it doesn't error - actual display is side effect
    (should-not (condition-case err
                    (progn
                      (cl-letf (((symbol-function 'xref-show-xrefs)
                                 (lambda (fetcher _display-action)
                                   (funcall fetcher))))
                        (sly-xref-query :calls "foo"))
                      nil)
                  (error err)))))

(ert-deftest sly-xref-unit--query-not-implemented ()
  "Test sly-xref-query when Lisp returns :not-implemented."
  (skip-unless (fboundp 'sly-xref-query))
  (sly-test-with-mock-eval
      ;; Note: sly-xref-query uses `(slynk:xref ',type ',symbol) which quotes args
      (((slynk:xref ':binds '"foo") . :not-implemented))
    ;; Should message, not error
    (cl-letf (((symbol-function 'sly-lisp-implementation-name)
               (lambda () "MockLisp")))
      (should-not (condition-case err
                      (progn (sly-xref-query :binds "foo") nil)
                    (error err))))))

(ert-deftest sly-xref-unit--query-all-types-registered ()
  "Test that all expected query types are registered."
  (skip-unless (boundp 'sly-xref-query-types))
  (let ((expected-types '(:calls :calls-who :references :binds :sets
                          :macroexpands :specializes :callers :callees)))
    (dolist (type expected-types)
      (should (cl-find type sly-xref-query-types :key #'cdr)))))

;;;; Recompile Mode Tests
;;;;

(ert-deftest sly-xref-unit--recompile-mode-map-defined ()
  "Test that recompile mode keymap has expected bindings."
  (skip-unless (boundp 'sly-xref-recompile-mode-map))
  (should (keymapp sly-xref-recompile-mode-map))
  (should (lookup-key sly-xref-recompile-mode-map (kbd "C-c C-c")))
  (should (lookup-key sly-xref-recompile-mode-map (kbd "C-c C-k"))))

;;;; Backward Compatibility Tests
;;;;

(ert-deftest sly-xref-unit--deprecated-edit-definition-works ()
  "Test that deprecated sly-edit-definition still works."
  (skip-unless (fboundp 'sly-edit-definition))
  (sly-test-with-mock-eval
      (((slynk:find-definitions-for-emacs "car")
        . (("(defun car)" (:location (:file "/tmp/cl.lisp") (:line 1) nil)))))
    ;; Should not error
    (cl-letf (((symbol-function 'xref-push-marker-stack) #'ignore)
              ((symbol-function 'xref-pop-marker-stack) #'ignore)
              ((symbol-function 'xref-show-xrefs)
               (lambda (fetcher _action) (funcall fetcher)))
              ((symbol-function 'sly--pop-to-source-location) #'ignore))
      (should-not (condition-case err
                      (progn (sly-edit-definition "car") nil)
                    (error err))))))

(ert-deftest sly-xref-unit--deprecated-who-calls-works ()
  "Test that deprecated sly-who-calls still works via sly-xref."
  (skip-unless (fboundp 'sly-who-calls))
  (sly-test-with-mock-eval
      ;; Note: sly-xref uses `(slynk:xref ',type ',symbol) which quotes args
      (((slynk:xref ':calls '"foo")
        . (("(defun bar)" (:location (:file "/tmp/bar.lisp") (:line 5) nil)))))
    (cl-letf (((symbol-function 'sly-xref--show-results)
               (lambda (&rest _args) t)))
      (should-not (condition-case err
                      (progn (sly-who-calls "foo") nil)
                    (error err))))))

(provide 'sly-xref-unit-tests)

;;; sly-xref-unit-tests.el ends here
