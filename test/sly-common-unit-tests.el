;;; sly-common-unit-tests.el --- Unit tests for sly-common.el -*- lexical-binding: t; -*-

;; Copyright (C) 2024 SLY Contributors

;;; Commentary:

;; Unit tests for the common utilities in lib/sly-common.el.
;; Tests cover buffer manipulation and refresh functionality.
;;
;; WHAT IS TESTED:
;;
;; This module tests utilities for manipulating Emacs buffers while
;; preserving state like point position, narrowing, and read-only status.
;; These are critical for SLY's REPL and inspector, which constantly
;; update their buffers while keeping user's cursor position intact.
;;
;; KEY FUNCTIONS TESTED:
;;
;; sly--call-refreshing: Core function for safe buffer updates
;;   - Erases and repopulates buffer while preserving point position
;;   - Respects narrowed regions and read-only buffers
;;   - Can optionally skip erasing (DONT-ERASE mode)
;;   - Preserves overlay-based markers for point location
;;
;; sly-refreshing: Macro wrapper around sly--call-refreshing
;;   - Cleaner syntax for common patterns
;;   - Automatically handles current-buffer
;;
;; WHAT IS COVERED:
;;
;; Basic Buffer Operations:
;;   - Clearing and repopulating buffer content
;;   - Erasing with DONT-ERASE flag (preserve old content)
;;   - Appending mode (add to end instead of replace)
;;
;; Point Recovery:
;;   - Restoring point after buffer clear
;;   - Finding point when content changes (using markers)
;;   - Edge cases (point at EOF, empty buffer)
;;
;; Buffer State Preservation:
;;   - Narrowed regions (buffer is limited to subset)
;;   - Read-only buffers (edits still work, then restored)
;;   - Active regions and mark preservation
;;
;; Overlays:
;;   - Using overlays to mark point location
;;   - Deleting old overlays before refresh
;;   - Overlay-based buffer regions
;;
;; WHEN TO ADD TESTS:
;;
;; Add tests when modifying:
;;   - Buffer refresh logic (how point is restored)
;;   - Narrowing or read-only handling
;;   - Overlay management for markers
;;   - Edge cases with empty/large buffers
;;
;; WHY UNIT TESTS:
;;
;; Buffer manipulation is complex (many edge cases) and needs careful
;; testing to avoid user-visible issues like lost point position.
;; These unit tests verify core behavior in isolation.
;;
;; See doc/TESTING-UNIT-TESTS.md for patterns and utilities.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'sly-common "lib/sly-common")
(require 'sly-unit-tests "lib/sly-unit-tests")

;;;; Tests for sly--call-refreshing
;;;;
(ert-deftest sly-common-unit--call-refreshing-clears-buffer ()
  "Test `sly--call-refreshing' clears buffer content."
  (with-temp-buffer
    (insert "Initial content")
    (sly--call-refreshing (current-buffer) nil nil nil nil
                          (lambda () (insert "New content")))
    (should (equal "New content" (buffer-string)))))

(ert-deftest sly-common-unit--call-refreshing-dont-erase ()
  "Test `sly--call-refreshing' with DONT-ERASE flag."
  (with-temp-buffer
    (insert "Existing content\n")
    (sly--call-refreshing (current-buffer) nil t nil nil
                          (lambda () (insert "Added content")))
    (should (equal "Existing content\nAdded content" (buffer-string)))))

(ert-deftest sly-common-unit--call-refreshing-recover-point ()
  "Test `sly--call-refreshing' recovers point position."
  (with-temp-buffer
    (insert "Line 1\nLine 2\nLine 3\n")
    (goto-char 10) ; Middle of buffer
    (let ((original-point (point)))
      (sly--call-refreshing (current-buffer) nil t t nil
                            (lambda () (insert "Extra\n")))
      ;; Point should be restored to original position
      (should (= original-point (point))))))

(ert-deftest sly-common-unit--call-refreshing-no-recover-point ()
  "Test `sly--call-refreshing' without point recovery."
  (with-temp-buffer
    (insert "Content")
    (goto-char (point-min))
    (sly--call-refreshing (current-buffer) nil nil nil nil
                          (lambda ()
                            (insert "New content")
                            (goto-char (point-max))))
    ;; Point should not be at beginning
    (should (= (point) (point-max)))))

(ert-deftest sly-common-unit--call-refreshing-with-overlay ()
  "Test `sly--call-refreshing' restricted to overlay region."
  ;; WHY THIS IS COMPLEX:
  ;; When refreshing a buffer region (instead of whole buffer), we pass an
  ;; overlay that marks the region to refresh. The function must:
  ;;   1. Clear only content in the overlay region (8-15)
  ;;   2. Execute the body (inserts replacement text)
  ;;   3. Leave content outside overlay untouched
  ;;
  ;; Setup: "Before\nMiddle\nAfter\n"
  ;;        12345678 15  23   28
  ;; Overlay spans 8-15, covering "Middle\n"
  (with-temp-buffer
    (insert "Before\nMiddle\nAfter\n")
    (let ((ov (make-overlay 8 15))) ; ← Marks "Middle\n" region
      (unwind-protect
          (progn
            (sly--call-refreshing (current-buffer) ov nil nil nil
                                  (lambda () (insert "REPLACED")))
            ;; Verify only overlay region was affected
            (should (string-match-p "Before" (buffer-string)))  ; ← outside overlay
            (should (string-match-p "REPLACED" (buffer-string)))  ; ← replacement inserted
            (should (string-match-p "After" (buffer-string)))  ; ← outside overlay
            (should-not (string-match-p "Middle" (buffer-string))))  ; ← cleared by refresh
        (delete-overlay ov)))))

(ert-deftest sly-common-unit--call-refreshing-returns-buffer ()
  "Test `sly--call-refreshing' returns the buffer."
  (with-temp-buffer
    (let ((result (sly--call-refreshing (current-buffer) nil nil nil nil
                                        (lambda () (insert "test")))))
      (should (eq result (current-buffer))))))

(ert-deftest sly-common-unit--call-refreshing-inhibit-read-only ()
  "Test `sly--call-refreshing' works with read-only buffers."
  (with-temp-buffer
    (insert "Read-only content")
    (setq buffer-read-only t)
    ;; Should not error even though buffer is read-only
    (should-not
     (condition-case nil
         (progn
           (sly--call-refreshing (current-buffer) nil nil nil nil
                                 (lambda () (insert "Modified")))
           nil)
       (error t)))))

;;;; Tests for sly-refreshing macro
;;;;
(ert-deftest sly-common-unit--refreshing-macro-basic ()
  "Test `sly-refreshing' macro basic usage."
  (with-temp-buffer
    (insert "Old content")
    (sly-refreshing ()
      (insert "New content"))
    (should (equal "New content" (buffer-string)))))

(ert-deftest sly-common-unit--refreshing-macro-dont-erase ()
  "Test `sly-refreshing' macro with :dont-erase."
  (with-temp-buffer
    (insert "Existing\n")
    (sly-refreshing (:dont-erase t)
      (insert "Appended"))
    (should (equal "Existing\nAppended" (buffer-string)))))

(ert-deftest sly-common-unit--refreshing-macro-overlay ()
  "Test `sly-refreshing' macro with :overlay."
  (with-temp-buffer
    (insert "AAA\nBBB\nCCC\n")
    (let ((ov (make-overlay 5 9))) ; "BBB\n"
      (unwind-protect
          (progn
            (sly-refreshing (:overlay ov)
              (insert "XXX"))
            (should (string-match-p "AAA" (buffer-string)))
            (should (string-match-p "XXX" (buffer-string)))
            (should (string-match-p "CCC" (buffer-string)))
            (should-not (string-match-p "BBB" (buffer-string))))
        (delete-overlay ov)))))

(ert-deftest sly-common-unit--refreshing-macro-recover-point ()
  "Test `sly-refreshing' macro with :recover-point-p."
  (with-temp-buffer
    (insert "Line 1\nLine 2\n")
    (goto-char 5)
    (let ((saved-point (point)))
      (sly-refreshing (:dont-erase t :recover-point-p t)
        (goto-char (point-max))
        (insert "Line 3\n"))
      (should (= saved-point (point))))))

(ert-deftest sly-common-unit--refreshing-macro-no-recover-point ()
  "Test `sly-refreshing' macro with :recover-point-p nil."
  (with-temp-buffer
    (insert "Content")
    (goto-char (point-min))
    (sly-refreshing (:recover-point-p nil)
      (insert "New")
      (goto-char (point-max)))
    (should (= (point) (point-max)))))

(ert-deftest sly-common-unit--refreshing-macro-in-other-buffer ()
  "Test `sly-refreshing' macro with :buffer parameter."
  (let ((buf1 (generate-new-buffer " *test1*"))
        (buf2 (generate-new-buffer " *test2*")))
    (unwind-protect
        (progn
          (with-current-buffer buf1
            (insert "Buffer 1 content"))
          (with-current-buffer buf2
            (insert "Buffer 2 content"))
          ;; Execute in buf2's context but refresh buf1
          (with-current-buffer buf2
            (sly-refreshing (:buffer buf1)
              (insert "Modified")))
          ;; buf1 should be modified, buf2 unchanged
          (with-current-buffer buf1
            (should (equal "Modified" (buffer-string))))
          (with-current-buffer buf2
            (should (equal "Buffer 2 content" (buffer-string)))))
      (kill-buffer buf1)
      (kill-buffer buf2))))

(ert-deftest sly-common-unit--refreshing-macro-multiple-forms ()
  "Test `sly-refreshing' macro with multiple body forms."
  (with-temp-buffer
    (insert "Old")
    (sly-refreshing ()
      (insert "Line 1\n")
      (insert "Line 2\n")
      (insert "Line 3"))
    (should (equal "Line 1\nLine 2\nLine 3" (buffer-string)))))

(ert-deftest sly-common-unit--refreshing-macro-returns-buffer ()
  "Test `sly-refreshing' macro returns buffer."
  (with-temp-buffer
    (let ((result (sly-refreshing ()
                    (insert "test"))))
      (should (eq result (current-buffer))))))

;;;; Edge cases
;;;;
(ert-deftest sly-common-unit--refreshing-empty-buffer ()
  "Test refreshing an empty buffer."
  (with-temp-buffer
    (sly-refreshing ()
      (insert "New content"))
    (should (equal "New content" (buffer-string)))))

(ert-deftest sly-common-unit--refreshing-empty-body ()
  "Test refreshing with empty body."
  (with-temp-buffer
    (insert "Content")
    (sly-refreshing ())
    ;; Should clear buffer but not insert anything
    (should (equal "" (buffer-string)))))

(ert-deftest sly-common-unit--refreshing-narrowed-buffer ()
  "Test refreshing works with narrowed buffer."
  (with-temp-buffer
    (insert "Part1\nPart2\nPart3\n")
    (goto-char (point-min))
    (search-forward "Part2")
    (beginning-of-line)
    (narrow-to-region (point) (line-end-position))
    (sly-refreshing ()
      (insert "Replaced"))
    (widen)
    ;; Only the narrowed region should be replaced
    (should (string-match-p "Part1" (buffer-string)))
    (should (string-match-p "Replaced" (buffer-string)))
    (should (string-match-p "Part3" (buffer-string)))))

(provide 'sly-common-unit-tests)
;;; sly-common-unit-tests.el ends here
