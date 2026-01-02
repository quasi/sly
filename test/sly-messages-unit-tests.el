;;; sly-messages-unit-tests.el --- Unit tests for sly-messages.el -*- lexical-binding: t; -*-

;; Copyright (C) 2024 SLY Contributors

;;; Commentary:

;; Unit tests for the messaging utilities in lib/sly-messages.el.
;; Tests cover pure Elisp functionality that doesn't require timers or user interaction.
;;
;; WHAT IS TESTED:
;;
;; This module tests SLY's system for communicating with the user—messages,
;; warnings, errors, and visual feedback.  Good messaging is critical for
;; user experience: users need clear feedback about what's happening and
;; what went wrong, without overwhelming them with noise.
;;
;; KEY FUNCTIONS TESTED:
;;
;; sly-message: Informational messages
;;   - Display in echo area and message buffer
;;   - Prefix with [sly] tag
;;   - Truncate long messages
;;   - Oneliners (first line only)
;;
;; sly-warning: Warning messages
;;   - Styled as warnings
;;   - Prefix appropriately
;;   - Different from info (user should notice)
;;
;; sly-error: Error messages
;;   - Style indicates error occurred
;;   - Prefix appropriately
;;   - Clear indication of problem
;;
;; sly-user-error: Errors caused by user action (not SLY bug)
;;   - Styled as user error (not internal error)
;;   - Clear and actionable message
;;
;; sly-flash-region: Visual feedback
;;   - Highlight a region temporarily
;;   - Provides visual confirmation
;;
;; WHAT IS COVERED:
;;
;; Message Formatting:
;;   - Prefix messages with [sly]
;;   - Oneliners (truncate at newline)
;;   - Long message truncation
;;   - Empty message handling
;;
;; Message Levels:
;;   - Info/message level (normal)
;;   - Warning level (user should notice)
;;   - Error level (something failed)
;;   - User-error (user caused the problem)
;;
;; Prompt Formatting:
;;   - Formatting prompts with [sly] prefix
;;   - Handling newlines and whitespace
;;   - Prompt display in minibuffer
;;
;; Visual Feedback:
;;   - Flashing regions for highlights
;;   - Temporary highlighting
;;   - Face application
;;
;; WHEN TO ADD TESTS:
;;
;; Add tests when modifying:
;;   - Message formatting or prefixing
;;   - Message level distinctions
;;   - Truncation behavior
;;   - Visual feedback (flashing)
;;   - Prompt formatting
;;
;; WHY UNIT TESTS:
;;
;; Message formatting must be consistent (users rely on visual patterns
;; to understand messages quickly) and robust (handle edge cases like
;; empty messages, very long messages, special characters). Unit tests
;; verify formatting works correctly.
;;
;; See doc/TESTING-UNIT-TESTS.md for patterns and utilities.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'sly-messages "lib/sly-messages")
(require 'sly-unit-tests "lib/sly-unit-tests")

;;;; Tests for sly-oneliner
;;;;
(ert-deftest sly-messages-unit--oneliner-short-string ()
  "Test `sly-oneliner' with string shorter than window width."
  (let ((input "Short message"))
    (should (equal input (sly-oneliner input)))))

(ert-deftest sly-messages-unit--oneliner-with-newline ()
  "Test `sly-oneliner' truncates at newline."
  (let ((input "First line\nSecond line\nThird line"))
    (should (equal "First line" (sly-oneliner input)))))

(ert-deftest sly-messages-unit--oneliner-empty-string ()
  "Test `sly-oneliner' with empty string."
  (should (equal "" (sly-oneliner ""))))

(ert-deftest sly-messages-unit--oneliner-single-newline ()
  "Test `sly-oneliner' with string that's just a newline."
  (should (equal "" (sly-oneliner "\n"))))

(ert-deftest sly-messages-unit--oneliner-multiple-newlines ()
  "Test `sly-oneliner' with multiple consecutive newlines."
  (should (equal "" (sly-oneliner "\n\n\n"))))

(ert-deftest sly-messages-unit--oneliner-newline-at-end ()
  "Test `sly-oneliner' with newline at end."
  (let ((input "Message with trailing newline\n"))
    (should (equal "Message with trailing newline" (sly-oneliner input)))))

(ert-deftest sly-messages-unit--oneliner-very-long-line ()
  "Test `sly-oneliner' truncates very long line to window width."
  (let* ((window-width (window-width (minibuffer-window)))
         (long-string (make-string (* 2 window-width) ?x))
         (result (sly-oneliner long-string)))
    ;; Should be truncated to less than window width
    (should (< (length result) window-width))
    ;; Should be exactly window-width - 1
    (should (= (length result) (1- window-width)))))

;;;; Tests for sly-message
;;;;
(ert-deftest sly-messages-unit--message-format ()
  "Test `sly-message' adds [sly] prefix."
  (let ((sly--last-message nil))
    (sly-unit-with-mock ((message (lambda (fmt &rest args)
                                     (apply #'format fmt args))))
      (sly-message "Test message")
      (should (equal "[sly] Test message" sly--last-message)))))

(ert-deftest sly-messages-unit--message-with-args ()
  "Test `sly-message' with format arguments."
  (let ((sly--last-message nil))
    (sly-unit-with-mock ((message (lambda (fmt &rest args)
                                     (apply #'format fmt args))))
      (sly-message "Value: %s, Number: %d" "foo" 42)
      (should (equal "[sly] Value: foo, Number: 42" sly--last-message)))))

(ert-deftest sly-messages-unit--message-updates-last-message ()
  "Test `sly-message' updates `sly--last-message'."
  (let ((sly--last-message nil))
    (sly-unit-with-mock ((message (lambda (fmt &rest args)
                                     (apply #'format fmt args))))
      (sly-message "First message")
      (should (equal "[sly] First message" sly--last-message))
      (sly-message "Second message")
      (should (equal "[sly] Second message" sly--last-message)))))

;;;; Tests for sly--message-clear-last-message
;;;;
(ert-deftest sly-messages-unit--clear-last-message ()
  "Test `sly--message-clear-last-message' clears state."
  (let ((sly--last-message "[sly] Some message"))
    (sly--message-clear-last-message)
    (should (null sly--last-message))))

;;;; Tests for sly-error
;;;;
(ert-deftest sly-messages-unit--error-format ()
  "Test `sly-error' adds [sly] prefix to error."
  (should-error (sly-error "Something went wrong")
                :type 'error)
  (condition-case err
      (sly-error "Error: %s" "bad input")
    (error
     (should (string-match-p "\\[sly\\] Error: bad input"
                             (error-message-string err))))))

(ert-deftest sly-messages-unit--error-with-args ()
  "Test `sly-error' with format arguments."
  (condition-case err
      (sly-error "Failed at step %d: %s" 3 "timeout")
    (error
     (should (string-match-p "\\[sly\\] Failed at step 3: timeout"
                             (error-message-string err))))))

;;;; Tests for sly-user-error
;;;;
(ert-deftest sly-messages-unit--user-error-format ()
  "Test `sly-user-error' adds [sly] prefix."
  (should-error (sly-user-error "User mistake")
                :type 'user-error)
  (condition-case err
      (sly-user-error "Invalid input: %s" "foo")
    (user-error
     (should (string-match-p "\\[sly\\] Invalid input: foo"
                             (error-message-string err))))))

;;;; Tests for sly-warning
;;;;
(ert-deftest sly-messages-unit--warning-format ()
  "Test `sly-warning' formats warning message."
  (let ((warning-messages nil))
    (sly-unit-with-mock ((display-warning
                          (lambda (type message &optional level buffer-name)
                            (push (list type message level buffer-name)
                                  warning-messages))))
      (sly-warning "This is a warning")
      (should (equal 1 (length warning-messages)))
      (let ((logged (car warning-messages)))
        (should (equal '(sly warning) (nth 0 logged)))
        (should (equal "This is a warning" (nth 1 logged)))))))

(ert-deftest sly-messages-unit--warning-with-args ()
  "Test `sly-warning' with format arguments."
  (let ((warning-messages nil))
    (sly-unit-with-mock ((display-warning
                          (lambda (type message &optional level buffer-name)
                            (push (list type message) warning-messages))))
      (sly-warning "Warning at %s: %d" "point" 100)
      (should (equal 1 (length warning-messages)))
      (should (equal "Warning at point: 100"
                     (nth 1 (car warning-messages)))))))

;;;; Tests for sly-flash-region
;;;;
(ert-deftest sly-messages-unit--flash-inhibit ()
  "Test `sly-flash-region' respects `sly-flash-inhibit'."
  (with-temp-buffer
    (insert "Some text to flash")
    (let ((sly-flash-inhibit t)
          (overlay-moved nil))
      (sly-unit-with-mock ((move-overlay
                            (lambda (&rest args)
                              (setq overlay-moved t)
                              nil)))
        (sly-flash-region 1 10)
        ;; When inhibited, overlay should not be moved
        (should-not overlay-moved)))))

(ert-deftest sly-messages-unit--flash-pattern-validation ()
  "Test `sly-flash-region' validates mutually exclusive args."
  (with-temp-buffer
    (insert "Test text")
    ;; Providing both PATTERN and TIMES should signal an error
    (should-error (sly-flash-region 1 5 :pattern '(0.1) :times 2))
    ;; Providing both PATTERN and TIMEOUT should signal an error
    (should-error (sly-flash-region 1 5 :pattern '(0.1) :timeout 0.5))))

;;;; Tests for sly-display-oneliner
;;;;
(ert-deftest sly-messages-unit--display-oneliner-not-in-minibuffer ()
  "Test `sly-display-oneliner' when not in minibuffer."
  (let ((sly--last-message nil)
        (messages-logged nil))
    (sly-unit-with-mock ((minibuffer-window-active-p (lambda (&rest args) nil))
                         (message (lambda (fmt &rest args)
                                    (push (apply #'format fmt args)
                                          messages-logged)
                                    nil)))
      (sly-display-oneliner "Test message %s" "here")
      (should sly--last-message)
      (should (string-match-p "Test message here" sly--last-message)))))

(ert-deftest sly-messages-unit--display-oneliner-in-minibuffer ()
  "Test `sly-display-oneliner' suppressed when in minibuffer."
  (let ((sly--last-message nil))
    (sly-unit-with-mock ((minibuffer-window-active-p (lambda (&rest args) t))
                         (message (lambda (fmt &rest args)
                                    (error "Should not call message"))))
      ;; Should not call message when in minibuffer
      (sly-display-oneliner "Should be suppressed")
      (should-not sly--last-message))))

(ert-deftest sly-messages-unit--display-oneliner-truncates ()
  "Test `sly-display-oneliner' truncates long messages."
  (let ((sly--last-message nil))
    (sly-unit-with-mock ((minibuffer-window-active-p (lambda (&rest args) nil))
                         (message (lambda (fmt &rest args) nil)))
      (sly-display-oneliner "First line\nSecond line\nThird line")
      ;; Should have truncated at first newline
      (should (string-match-p "First line" sly--last-message))
      (should-not (string-match-p "Second line" sly--last-message)))))

;;;; Tests for sly-y-or-n-p
;;;;
(ert-deftest sly-messages-unit--y-or-n-p-format ()
  "Test `sly-y-or-n-p' adds [sly] prefix to prompt."
  (let ((captured-prompt nil))
    (sly-unit-with-mock ((y-or-n-p (lambda (prompt)
                                      (setq captured-prompt prompt)
                                      t)))
      (sly-y-or-n-p "Continue?")
      (should (equal "[sly] Continue?" captured-prompt)))))

(ert-deftest sly-messages-unit--y-or-n-p-with-args ()
  "Test `sly-y-or-n-p' with format arguments."
  (let ((captured-prompt nil))
    (sly-unit-with-mock ((y-or-n-p (lambda (prompt)
                                      (setq captured-prompt prompt)
                                      nil)))
      (sly-y-or-n-p "Delete %d files?" 42)
      (should (equal "[sly] Delete 42 files?" captured-prompt)))))

(provide 'sly-messages-unit-tests)
;;; sly-messages-unit-tests.el ends here
