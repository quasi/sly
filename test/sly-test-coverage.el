;;; sly-test-coverage.el --- Coverage setup for SLY tests -*- lexical-binding: t; -*-

;; Copyright (C) 2024 SLY Contributors

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Code coverage configuration for SLY using undercover.el.
;; This file should be loaded before running tests to enable coverage tracking.
;;
;; Usage:
;;   make check-unit-coverage    # Run unit tests with coverage
;;   make check-coverage         # Run all tests with coverage
;;
;; Coverage reports are generated in coverage/ directory.

;;; Code:

(when (require 'undercover nil t)
  (undercover "sly.el"
              ;; Core library files
              "lib/sly-parse.el"
              "lib/sly-completion.el"
              "lib/sly-buttons.el"
              "lib/sly-messages.el"
              "lib/sly-common.el"
              "lib/sly-cl-indent.el"
              ;; Contrib files (optional - comment out if too slow)
              (:exclude "contrib/sly-*.el")
              ;; Report configuration
              (:report-format 'codecov)
              (:report-file "coverage/codecov.json")
              (:send-report nil)))  ; Don't auto-send, CI will handle upload

(provide 'sly-test-coverage)
;;; sly-test-coverage.el ends here
