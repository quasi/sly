;;; sly-xref.el --- xref.el backend for SLY -*- lexical-binding: t; -*-

;; Copyright (C) 2026 SLY Contributors

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; This file provides integration between SLY and Emacs's built-in xref.el
;; framework for cross-reference navigation.
;;
;; WHAT THIS FILE PROVIDES:
;;
;; When SLY is connected to a Lisp, this module registers an xref backend
;; that enables standard Emacs navigation commands to work with Lisp code:
;;
;;   M-.   (xref-find-definitions)  - Jump to definition
;;   M-,   (xref-go-back)           - Return from definition
;;   M-?   (xref-find-references)   - Find callers
;;
;; Additionally, SLY's extended cross-reference queries (who-calls,
;; who-binds, who-sets, etc.) are available via the unified command
;; `sly-xref-query' (bound to C-c C-w).
;;
;; KEY COMPONENTS:
;;
;; Backend Registration:
;;   sly-xref-backend: Returns 'sly when connected, enabling xref dispatch
;;
;; Backend Methods:
;;   xref-backend-identifier-at-point: Extract Lisp symbol at point
;;   xref-backend-definitions: Find definitions via Slynk
;;   xref-backend-references: Find callers via Slynk
;;   xref-backend-apropos: Pattern-based symbol search
;;
;; Location Conversion:
;;   sly-xref--convert-to-xref-items: Convert Slynk results to xref objects
;;   sly-xref--convert-location: Handle various Slynk location formats
;;   sly-xref--position-to-line-col: Byte position to line/column
;;
;; Query System:
;;   sly-xref-query: Unified command for all xref query types
;;   sly-xref-query-types: Registry mapping names to Slynk keywords
;;
;; Recompile Support:
;;   sly-xref-recompile-mode: Minor mode adding recompile to xref buffers
;;
;; ARCHITECTURE:
;;
;; This module acts as a bridge between two systems:
;;
;; 1. Emacs xref.el expects:
;;    - xref-item objects with summary strings
;;    - xref-file-location or xref-buffer-location objects
;;    - Synchronous return values from backend methods
;;
;; 2. Slynk returns:
;;    - Lists of (DSPEC LOCATION) pairs
;;    - Locations as (:location BUFFER-SPEC POSITION-SPEC HINTS)
;;    - Position specs as byte offsets, line numbers, or function names
;;
;; The conversion functions handle translating between these formats,
;; including reading files to convert byte positions to line/column.

;;; Code:

(require 'xref)
(require 'cl-lib)

;; This file is loaded by sly.el after all core functions are defined.
;; We use forward declarations to avoid circular dependencies during
;; byte-compilation.
(declare-function sly-connected-p "sly")
(declare-function sly-eval "sly")
(declare-function sly-symbol-at-point "sly")
(declare-function sly-read-symbol-name "sly")
(declare-function sly-current-package "sly")
(declare-function sly-lisp-implementation-name "sly")
(declare-function sly-location-p "sly")
(declare-function sly--pop-to-source-location "sly")
(declare-function sly-recompile-locations "sly")
(declare-function sly-compilation-finished "sly")
(declare-function sly-aggregate-compilation-results "sly")
(declare-function sly-simple-completions "lib/sly-completion")

;; sly-dcase is a macro defined in sly.el
(eval-when-compile
  (require 'sly nil t))


;;;; Backend Registration
;;;;

;;;###autoload
(defun sly-xref-backend ()
  "SLY backend for xref.el.
Returns \\='sly when SLY is connected, nil otherwise."
  (when (and (fboundp 'sly-connected-p) (sly-connected-p))
    'sly))

;;;###autoload
(with-eval-after-load 'xref
  (add-hook 'xref-backend-functions #'sly-xref-backend))


;;;; Location Conversion
;;;;

(defun sly-xref--convert-to-xref-items (slynk-xrefs)
  "Convert Slynk xrefs to xref-item objects.
SLYNK-XREFS is a list of (DSPEC LOCATION) pairs from Slynk."
  (cl-loop for (dspec location) in slynk-xrefs
           for xref-loc = (sly-xref--convert-location location)
           when xref-loc
           collect (xref-make (if (stringp dspec)
                                  dspec
                                (prin1-to-string dspec))
                              xref-loc)))

(defun sly-xref--convert-location (location)
  "Convert a Slynk LOCATION to an xref location object.
Returns nil if LOCATION cannot be converted."
  (when location
    (sly-dcase location
      ((:location buffer position _hints)
       (sly-xref--make-location buffer position))
      ((:error message)
       (xref-make-bogus-location message))
      (t nil))))

(defun sly-xref--make-location (buffer-spec position-spec)
  "Create xref location from Slynk BUFFER-SPEC and POSITION-SPEC."
  (sly-dcase buffer-spec
    ((:file filename)
     (let ((line-col (sly-xref--position-to-line-col filename position-spec)))
       (xref-make-file-location filename (car line-col) (cdr line-col))))
    ((:buffer buffer-name)
     (if-let ((buf (get-buffer buffer-name)))
         (let ((pos (sly-xref--resolve-position position-spec buf)))
           (xref-make-buffer-location buf pos))
       (xref-make-bogus-location (format "Buffer %s not found" buffer-name))))
    ((:buffer-and-file _buffer-name filename)
     ;; Prefer file location for persistence
     (let ((line-col (sly-xref--position-to-line-col filename position-spec)))
       (xref-make-file-location filename (car line-col) (cdr line-col))))
    ((:source-form _)
     (xref-make-bogus-location "Source form (no file location)"))
    ((:zip _archive-path _entry-path)
     (xref-make-bogus-location "Archive location not supported"))
    (t
     (xref-make-bogus-location "Unknown location type"))))

(defun sly-xref--position-to-line-col (filename position-spec)
  "Convert POSITION-SPEC to (LINE . COLUMN) for FILENAME.
Returns (1 . 0) if conversion fails."
  ;; For :line specs, we already have line/col - no need to read file
  (sly-dcase position-spec
    ((:line line &optional col)
     ;; xref.el uses 0-based columns; Slynk uses 1-based
     (cons line (if col (1- col) 0)))
    (t
     ;; For other position types, read file to compute line/column
     (condition-case nil
         (let ((pos (sly-xref--resolve-position-in-file filename position-spec)))
           (with-temp-buffer
             (insert-file-contents filename)
             (goto-char (min pos (point-max)))
             (cons (line-number-at-pos) (current-column))))
       (error (cons 1 0))))))

(defun sly-xref--resolve-position-in-file (filename position-spec)
  "Resolve POSITION-SPEC to a byte offset for FILENAME."
  (sly-dcase position-spec
    ((:position pos) pos)
    ((:offset _start pos) pos)
    ((:line line &optional col)
     (with-temp-buffer
       (insert-file-contents filename)
       (goto-char (point-min))
       (forward-line (1- line))
       (when col (forward-char (1- col)))
       (point)))
    ((:function-name name)
     (with-temp-buffer
       (insert-file-contents filename)
       (goto-char (point-min))
       (if (search-forward name nil t)
           (match-beginning 0)
         1)))
    (t 1)))

(defun sly-xref--resolve-position (position-spec buffer)
  "Resolve POSITION-SPEC to a point in BUFFER."
  (with-current-buffer buffer
    (sly-dcase position-spec
      ((:position pos) (min pos (point-max)))
      ((:offset _start pos) (min pos (point-max)))
      ((:line line &optional col)
       (save-excursion
         (goto-char (point-min))
         (forward-line (1- line))
         (when col (forward-char (min (1- col) (- (line-end-position) (point)))))
         (point)))
      ((:function-name name)
       (save-excursion
         (goto-char (point-min))
         (if (search-forward name nil t)
             (match-beginning 0)
           (point-min))))
      (t (point-min)))))


;;;; Backend Methods
;;;;

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql sly)))
  "Return the Lisp symbol at point as a string."
  (sly-symbol-at-point))

(cl-defmethod xref-backend-definitions ((_backend (eql sly)) identifier)
  "Find definitions of IDENTIFIER using Slynk."
  (when identifier
    (let ((xrefs (sly-eval `(slynk:find-definitions-for-emacs ,identifier))))
      (sly-xref--convert-to-xref-items xrefs))))

(cl-defmethod xref-backend-references ((_backend (eql sly)) identifier)
  "Find references to IDENTIFIER using Slynk.
This maps to Slynk's :callers query (portable, heap-groveling approach)."
  (when identifier
    (let ((xrefs (sly-eval `(slynk:xref ':callers ,identifier))))
      (unless (eq xrefs :not-implemented)
        (sly-xref--convert-to-xref-items xrefs)))))

(cl-defmethod xref-backend-apropos ((_backend (eql sly)) pattern)
  "Find symbols matching PATTERN."
  (when pattern
    (let ((results (sly-eval `(slynk:apropos-list-for-emacs ,pattern nil nil))))
      (cl-loop for (name . _rest) in results
               collect (xref-make name (xref-make-bogus-location name))))))

(cl-defmethod xref-backend-identifier-completion-table ((_backend (eql sly)))
  "Completion table for Lisp symbols."
  (when (fboundp 'sly-simple-completions)
    (completion-table-dynamic
     (lambda (prefix)
       (car (sly-simple-completions prefix))))))


;;;; Unified Xref Query Command
;;;;

(defvar sly-xref-query-types
  '(("calls (who calls this)" . :calls)
    ("calls-who (called by this)" . :calls-who)
    ("references (who references this)" . :references)
    ("binds (who binds this)" . :binds)
    ("sets (who sets this)" . :sets)
    ("macroexpands (who expands this)" . :macroexpands)
    ("specializes (methods on this class)" . :specializes)
    ("callers (portable who-calls)" . :callers)
    ("callees (portable calls-who)" . :callees))
  "Alist mapping xref query type descriptions to Slynk keywords.")

(defvar sly-xref--last-query-results nil
  "Results from the last `sly-xref-query', for recompile support.
This stores the raw Slynk results to enable recompilation.")

(defun sly-xref-query (type symbol)
  "Query for cross-references of TYPE for SYMBOL.
TYPE is a keyword like :calls, :references, etc.
Results are displayed in the standard *xref* buffer.

Interactively, prompts for the query type and symbol."
  (interactive
   (let* ((type-name (completing-read "Xref type: " sly-xref-query-types nil t))
          (type (cdr (assoc type-name sly-xref-query-types)))
          (sym (sly-read-symbol-name (format "Xref %s for: " type-name))))
     (list type sym)))
  (let ((xrefs (sly-eval `(slynk:xref ',type ',symbol))))
    (cond
     ((eq xrefs :not-implemented)
      (message "%s is not implemented for %s"
               type (sly-lisp-implementation-name)))
     ((null xrefs)
      (message "No %s found for %s" type symbol))
     (t
      (let* ((items (sly-xref--convert-to-xref-items xrefs))
             (fetcher (lambda () items)))
        ;; Store results for recompile feature
        (setq sly-xref--last-query-results xrefs)
        (xref-show-xrefs fetcher nil))))))


;;;; Recompile Minor Mode
;;;;

(defvar sly-xref-recompile-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'sly-xref-recompile-at-point)
    (define-key map (kbd "C-c C-k") #'sly-xref-recompile-all)
    map)
  "Keymap for `sly-xref-recompile-mode'.")

(define-minor-mode sly-xref-recompile-mode
  "Minor mode adding SLY recompilation commands to xref buffers.

When enabled, you can recompile definitions directly from xref results:

\\{sly-xref-recompile-mode-map}"
  :lighter " SLY-Recomp"
  :keymap sly-xref-recompile-mode-map)

(defun sly-xref--maybe-enable-recompile-mode ()
  "Enable recompile mode in xref buffers when SLY is the backend."
  (when (and (derived-mode-p 'xref--xref-buffer-mode)
             sly-xref--last-query-results
             (fboundp 'sly-connected-p)
             (sly-connected-p))
    (sly-xref-recompile-mode 1)))

;; Hook into xref buffer creation
(add-hook 'xref-after-return-hook #'sly-xref--maybe-enable-recompile-mode)

(defun sly-xref--find-slynk-location-for-xref (xref-item)
  "Find the original Slynk location for XREF-ITEM.
Searches `sly-xref--last-query-results' for a matching location."
  (when sly-xref--last-query-results
    (let ((summary (xref-item-summary xref-item)))
      (cl-loop for (dspec location) in sly-xref--last-query-results
               when (equal (if (stringp dspec) dspec (prin1-to-string dspec))
                           summary)
               return location))))

(defun sly-xref-recompile-at-point ()
  "Recompile the definition at point in xref buffer."
  (interactive)
  (if-let* ((xref (xref--item-at-point))
            (slynk-loc (sly-xref--find-slynk-location-for-xref xref)))
      (if (sly-location-p slynk-loc)
          (sly-recompile-locations
           (list slynk-loc)
           (lambda (result)
             (sly-compilation-finished result nil)
             (message "Recompiled: %s" (xref-item-summary xref))))
        (user-error "No source location for this definition"))
    (user-error "No xref at point")))

(defun sly-xref-recompile-all ()
  "Recompile all definitions in the xref buffer."
  (interactive)
  (if-let ((locations (cl-loop for (_dspec loc) in sly-xref--last-query-results
                               when (sly-location-p loc)
                               collect loc)))
      (sly-recompile-locations
       locations
       (lambda (results)
         (sly-compilation-finished
          (sly-aggregate-compilation-results results) nil)
         (message "Recompiled %d definitions" (length locations))))
    (user-error "No valid locations to recompile")))


;;;; Notes
;;;;
;; This module provides integration with Emacs's xref.el framework.
;; The existing SLY xref commands in sly.el (sly-edit-definition,
;; sly-who-calls, etc.) continue to work as before. This module
;; adds:
;;
;; 1. An xref.el backend so standard commands like xref-find-definitions
;;    (M-.) work with SLY when connected to a Lisp.
;;
;; 2. A new `sly-xref-query' command that provides a unified interface
;;    to all xref query types, displaying results in the standard
;;    *xref* buffer.
;;
;; 3. A minor mode that adds recompilation support to xref buffers.


(provide 'sly-xref)

;;; sly-xref.el ends here
