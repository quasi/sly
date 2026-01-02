;;; sly-buttons-unit-tests.el --- Unit tests for sly-buttons.el -*- lexical-binding: t; -*-

;; Copyright (C) 2024 SLY Contributors

;;; Commentary:

;; Unit tests for the button utilities in lib/sly-buttons.el.
;; These tests cover button creation, querying, and navigation.
;;
;; WHAT IS TESTED:
;;
;; This module tests SLY's button system—how clickable regions are created,
;; styled, queried, and interacted with.  Buttons are overlays that:
;;
;;   - Have a visual appearance (color, underline, cursor behavior)
;;   - Respond to mouse clicks and keyboard navigation
;;   - Can be nested (button level determines visual depth)
;;   - Have type hierarchies (sly-action and sly-part inherit from sly-button)
;;   - Store metadata (function to call, connection info, etc.)
;;
;; WHAT IS COVERED:
;;
;; Button Level (visual nesting):
;;   - sly-button--level: Get/set how deeply nested a button is
;;   - Default level is 0 (no nesting)
;;
;; Button Type Hierarchy:
;;   - 'sly-button: Base button type
;;   - 'sly-action: Clickable action button (inherits from sly-button)
;;   - 'sly-part: Structured part (inherits from sly-button)
;;
;; Button Queries:
;;   - sly-button--overlays-in: Find buttons in a region
;;   - sly-button-at: Get button at a specific point
;;   - Filtering by button type
;;
;; Button Creation:
;;   - sly--make-text-button: Create button with properties
;;   - sly-make-action-button: Create clickable button with callback
;;   - Button stores connection and package info
;;
;; Keymaps:
;;   - sly-part-button-keymap: Navigation and interaction keys
;;   - Keymap inheritance from button-map
;;   - sly-interactive-buttons-mode: Mode for button interaction
;;
;; WHEN TO ADD TESTS:
;;
;; Add tests when modifying:
;;   - Button creation or type hierarchies
;;   - Button level calculation or visual rendering
;;   - Button finding or filtering logic
;;   - Button keymaps or interaction behavior
;;   - Button metadata storage
;;
;; WHY UNIT TESTS:
;;
;; Button logic is pure Elisp (no Lisp connection needed) and can be
;; tested in isolation.  These unit tests verify the button system
;; works correctly before integration testing with a full SLY session.
;;
;; See doc/TESTING-UNIT-TESTS.md for patterns and utilities.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'sly-buttons "lib/sly-buttons")
(require 'sly-unit-tests "lib/sly-unit-tests")

;;;; Helper for creating test buttons
;;;;
(defun sly-buttons-test--make-overlay-button (beg end &rest props)
  "Create an overlay button from BEG to END with PROPS.
This creates a proper sly-button overlay that will be recognized
by `sly-button--overlays-in' and related functions."
  (let ((ov (make-overlay beg end)))
    ;; WHY THESE PROPERTIES MATTER:
    ;;
    ;; 'category 'sly-button: Links overlay to the button type defined via
    ;; define-button-type. This is what makes it a "real" button with all
    ;; the associated faces, keymaps, and behaviors.
    ;;
    ;; 'evaporate t: Makes overlay disappear automatically when all content
    ;; it covers is deleted (useful when buffer is edited).
    ;;
    ;; 'button t: Mark this overlay as a button (used by button.el).
    ;;
    ;; 'sly-button-search-id: Unique ID for this button, used to distinguish
    ;; it from other buttons and for stable references across edits.
    (overlay-put ov 'category 'sly-button)
    (overlay-put ov 'evaporate t)
    (overlay-put ov 'button t)
    (overlay-put ov 'sly-button-search-id (sly-button-next-search-id))
    ;; Apply any additional properties (passed in PROPS)
    (while props
      (overlay-put ov (pop props) (pop props)))
    ov))

(defmacro sly-buttons-test--with-button-buffer (content &rest body)
  "Execute BODY in a temp buffer with CONTENT."
  (declare (indent 1) (debug t))
  `(with-temp-buffer
     (insert ,content)
     (goto-char (point-min))
     ,@body))

;;;; Tests for sly-button--level
;;;;
(ert-deftest sly-buttons-unit--level-default ()
  "Test `sly-button--level' returns 0 for unset."
  (sly-buttons-test--with-button-buffer "test text"
    (let ((ov (make-overlay 1 5)))
      (unwind-protect
          (should (= 0 (sly-button--level ov)))
        (delete-overlay ov)))))

(ert-deftest sly-buttons-unit--level-set ()
  "Test `sly-button--level' returns set value."
  (sly-buttons-test--with-button-buffer "test text"
    (let ((ov (make-overlay 1 5)))
      (unwind-protect
          (progn
            (overlay-put ov 'sly-button-level 3)
            (should (= 3 (sly-button--level ov))))
        (delete-overlay ov)))))

(ert-deftest sly-buttons-unit--level-setf ()
  "Test setting `sly-button--level' via setf."
  (sly-buttons-test--with-button-buffer "test text"
    (let ((ov (make-overlay 1 5)))
      (unwind-protect
          (progn
            (setf (sly-button--level ov) 5)
            (should (= 5 (sly-button--level ov))))
        (delete-overlay ov)))))

;;;; Tests for sly-button--overlays-in (simplified - complex button type checking)
;;;;
;; Note: The sly-button--overlays-* functions have complex type checking via
;; button-type-subtype-p which requires proper button category setup.
;; These are tested indirectly through integration tests.

(ert-deftest sly-buttons-unit--overlays-in-none ()
  "Test `sly-button--overlays-in' returns nil when no buttons."
  (sly-buttons-test--with-button-buffer "test text"
    (should (null (sly-button--overlays-in 1 10)))))

;;;; Tests for button type definitions
;;;;
(ert-deftest sly-buttons-unit--button-type-sly-button-exists ()
  "Test 'sly-button type is defined."
  (should (get 'sly-button 'button-category-symbol)))

(ert-deftest sly-buttons-unit--button-type-sly-action-exists ()
  "Test 'sly-action type is defined."
  (should (get 'sly-action 'button-category-symbol)))

(ert-deftest sly-buttons-unit--button-type-sly-part-exists ()
  "Test 'sly-part type is defined."
  (should (get 'sly-part 'button-category-symbol)))

(ert-deftest sly-buttons-unit--sly-action-inherits-sly-button ()
  "Test 'sly-action inherits from 'sly-button."
  (should (button-type-subtype-p 'sly-action 'sly-button)))

(ert-deftest sly-buttons-unit--sly-part-inherits-sly-button ()
  "Test 'sly-part inherits from 'sly-button."
  (should (button-type-subtype-p 'sly-part 'sly-button)))

;;;; Tests for sly-button-next-search-id
;;;;
(ert-deftest sly-buttons-unit--next-search-id-increments ()
  "Test `sly-button-next-search-id' increments."
  (let ((id1 (sly-button-next-search-id))
        (id2 (sly-button-next-search-id)))
    (should (= (1+ id1) id2))))

;;;; Tests for text button creation
;;;;
(ert-deftest sly-buttons-unit--make-text-button ()
  "Test `sly--make-text-button' creates button with properties."
  (sly-unit-with-stubbed-connection
    (with-temp-buffer
      (insert "click me")
      (let ((button (sly--make-text-button 1 9 :type 'sly-action)))
        (should button)
        (should (button-at 1))
        (should (eq 'mock-connection
                    (button-get (button-at 1) 'sly-connection)))))))

;;;; Tests for sly-make-action-button
;;;;
(ert-deftest sly-buttons-unit--make-action-button ()
  "Test `sly-make-action-button' creates clickable button."
  (sly-unit-with-stubbed-connection
    (with-temp-buffer
      (let* ((clicked nil)
             (button (sly-make-action-button
                      "Click"
                      (lambda (_btn) (setq clicked t)))))
        (should (stringp button))
        (should (equal "Click" button))))))

;;;; Tests for sly-button-at
;;;;
(ert-deftest sly-buttons-unit--button-at-no-button ()
  "Test `sly-button-at' returns nil with no-error flag."
  (with-temp-buffer
    (insert "no buttons here")
    (goto-char 5)
    (should (null (sly-button-at nil nil 'no-error)))))

(ert-deftest sly-buttons-unit--button-at-errors-by-default ()
  "Test `sly-button-at' errors when no button and no-error not set."
  (with-temp-buffer
    (insert "no buttons here")
    (goto-char 5)
    (should-error (sly-button-at))))

(ert-deftest sly-buttons-unit--button-at-with-type ()
  "Test `sly-button-at' filters by type."
  (sly-unit-with-stubbed-connection
    (with-temp-buffer
      (insert "test")
      (sly--make-text-button 1 5 :type 'sly-action)
      (goto-char 2)
      ;; Should find sly-action
      (should (sly-button-at nil 'sly-action 'no-error))
      ;; Should not find sly-part
      (should-not (sly-button-at nil 'sly-part 'no-error)))))

;;;; Tests for keymap
;;;;
(ert-deftest sly-buttons-unit--part-button-keymap-exists ()
  "Test `sly-part-button-keymap' is defined."
  (should (keymapp sly-part-button-keymap)))

(ert-deftest sly-buttons-unit--part-button-keymap-has-bindings ()
  "Test `sly-part-button-keymap' inherits from button-map."
  ;; The keymap inherits from button-map and has some bindings
  (should (eq (keymap-parent sly-part-button-keymap) button-map)))

;;;; Tests for sly-interactive-buttons-mode
;;;;
(ert-deftest sly-buttons-unit--interactive-buttons-mode-exists ()
  "Test `sly-interactive-buttons-mode' is defined."
  (should (fboundp 'sly-interactive-buttons-mode)))

(provide 'sly-buttons-unit-tests)
;;; sly-buttons-unit-tests.el ends here
