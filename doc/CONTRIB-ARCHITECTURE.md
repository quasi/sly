# SLY Contrib System Architecture

This document provides a comprehensive guide to SLY's contrib (contribution) system, including how to understand existing contribs and how to create new ones.

## Overview

The contrib system allows SLY to be extended with optional features that can be loaded on demand. Each contrib can provide functionality on both the Emacs side (Elisp) and the SLYNK server side (Common Lisp).

**Key characteristics:**
- Contribs are optional - core SLY works without them
- Dependencies are automatically resolved
- Both sides (Emacs + Lisp) can be extended
- Hot loading/unloading is supported

---

## 1. Contrib Structure

### 1.1 File Organization

A typical contrib consists of:

```
contrib/
├── sly-<name>.el        # Emacs side (user-facing)
├── slynk-<name>.lisp    # SLYNK side (server, optional)
└── sly-<name>-tests.el  # Tests (in test/ directory)
```

**Naming conventions:**
- Emacs files: `sly-<name>.el`
- SLYNK files: `slynk-<name>.lisp`
- The `<name>` must match between paired files

### 1.2 Core Components

Each Emacs-side contrib defines itself using `define-sly-contrib`:

```elisp
(define-sly-contrib sly-example
  "One-line description of what this contrib does."
  (:authors "Author Name <email@example.com>")
  (:license "GPL")
  (:sly-dependencies sly-other-contrib)      ; Other Elisp contribs
  (:slynk-dependencies slynk/example)        ; SLYNK modules to load
  (:on-load (example-initialize))            ; Run when enabled
  (:on-unload (example-cleanup)))            ; Run when disabled
```

---

## 2. The Contrib Macro

### 2.1 `define-sly-contrib` Syntax

```elisp
(define-sly-contrib NAME DOCSTRING &rest OPTIONS)
```

**OPTIONS keywords:**

| Keyword | Value | Purpose |
|---------|-------|---------|
| `:authors` | string or list | Author information |
| `:license` | string | License identifier |
| `:sly-dependencies` | symbol or list | Required Elisp contribs |
| `:slynk-dependencies` | symbol or list | Required SLYNK modules |
| `:on-load` | form(s) | Code to run when enabling |
| `:on-unload` | form(s) | Code to run when disabling |

### 2.2 Generated Functions

The macro generates these functions:

```elisp
;; For a contrib named 'sly-example':
(defun sly-example-init ()
  "Enable sly-example contrib."
  ...)

(defun sly-example-unload ()
  "Disable sly-example contrib."
  ...)
```

### 2.3 Internal Data Structure

```elisp
;; sly.el:6911-6920
(cl-defstruct (sly-contrib (:conc-name sly-contrib.))
  enabled-p           ; Is this contrib currently enabled?
  name                ; Symbol naming the contrib
  sly-dependencies    ; List of required Elisp contribs
  slynk-dependencies  ; List of required SLYNK modules
  enable              ; Enable function
  disable             ; Disable function
  authors             ; Author string or list
  license)            ; License string
```

---

## 3. Lifecycle Management

### 3.1 Loading Sequence

When `sly-setup` is called (typically in user's init file):

```elisp
(sly-setup '(sly-fancy sly-mrepl))
```

The following happens:

```
1. Add contrib/ to load-path
       │
       ▼
2. For each contrib in list:
   ├── (require 'sly-<name>)
   └── Contrib struct registered
       │
       ▼
3. Calculate all dependencies (recursive)
       │
       ▼
4. Disable "forgotten" contribs
   (previously enabled, not in new list)
       │
       ▼
5. Enable new contribs (in dependency order)
   ├── Enable sly-dependencies first
   ├── Register slynk-dependencies
   ├── If connected: load SLYNK modules
   └── Run :on-load code
```

### 3.2 Enable Sequence (Detailed)

```elisp
;; sly.el:6948-6978 (simplified)
(defun sly-contrib--enable (contrib)
  ;; 1. Enable SLY dependencies first
  (dolist (dep (sly-contrib.sly-dependencies contrib))
    (sly-enable-contrib dep))

  ;; 2. Register SLYNK dependencies
  (dolist (dep (sly-contrib.slynk-dependencies contrib))
    (push dep sly-contrib--required-slynk-modules))

  ;; 3. Load SLYNK modules if connected
  (when (sly-connected-p)
    (sly-contrib--load-slynk-dependencies))

  ;; 4. Run :on-load code
  (funcall (sly-contrib.enable contrib))

  ;; 5. Mark as enabled
  (setf (sly-contrib.enabled-p contrib) t))
```

### 3.3 SLYNK Module Loading

```elisp
;; sly.el:6993-7010 (simplified)
(defun sly-contrib--load-slynk-dependencies ()
  "Load required SLYNK modules on current connection."
  (let ((modules-to-load sly-contrib--required-slynk-modules))
    (when modules-to-load
      (sly-eval-async
       `(slynk:slynk-require ',modules-to-load)
       (lambda (result)
         (message "Loaded SLYNK modules: %s" result))))))
```

On the SLYNK side, `slynk-require` handles loading:

```lisp
;; slynk.lisp:2991-3008
(defslyfun slynk-require (modules)
  "Load MODULES into the running Lisp."
  (dolist (module (ensure-list modules))
    (require-module module))
  *modules*)  ; Return list of loaded modules
```

### 3.4 Disable Sequence

```elisp
(defun sly-contrib--disable (contrib)
  ;; 1. Disable dependents first (reverse order)
  (dolist (dep (sly-contrib--dependents contrib))
    (sly-disable-contrib dep))

  ;; 2. Run :on-unload code
  (funcall (sly-contrib.disable contrib))

  ;; 3. Unregister SLYNK dependencies
  (dolist (dep (sly-contrib.slynk-dependencies contrib))
    (setq sly-contrib--required-slynk-modules
          (remove dep sly-contrib--required-slynk-modules)))

  ;; 4. Mark as disabled
  (setf (sly-contrib.enabled-p contrib) nil))
```

---

## 4. Existing Contribs

### 4.1 Meta-Contrib: sly-fancy

```elisp
;; contrib/sly-fancy.el
(define-sly-contrib sly-fancy
  "Load commonly used SLY contribs in one bundle."
  (:sly-dependencies sly-mrepl
                     sly-autodoc
                     sly-fancy-inspector
                     sly-fancy-trace
                     sly-scratch
                     sly-package-fu
                     sly-fontifying-fu
                     sly-trace-dialog
                     sly-stickers
                     sly-indentation
                     sly-tramp))
```

This is the default contrib loaded when SLY starts. It bundles the "nice-to-have" features.

### 4.2 sly-mrepl (Multiple REPLs)

**Purpose:** Provides multiple independent REPL buffers over a single connection.

**Files:**
- `contrib/sly-mrepl.el` (1,568 lines)
- `contrib/slynk-mrepl.lisp` (~500 lines)

**Key features:**
- Multiple REPL instances per connection
- Independent evaluation contexts
- comint.el-based UI with history
- Proper output routing

**Elisp highlights:**
```elisp
(define-sly-contrib sly-mrepl
  "Multiple REPLs over single connection."
  (:slynk-dependencies slynk/mrepl)
  (:on-load
   (add-hook 'sly-connected-hook 'sly-mrepl-on-connection))
  (:on-unload
   (remove-hook 'sly-connected-hook 'sly-mrepl-on-connection)))

(define-derived-mode sly-mrepl-mode comint-mode "mrepl"
  "Mode for SLY MREPL buffers."
  ...)
```

**SLYNK side:**
```lisp
;; Creates channel for REPL I/O
(defclass mrepl (channel)
  ((read-mode :initform :eval)
   (pending-errors :initform nil)))

(defmethod channel-send ((r mrepl) (selector (eql :eval)) args)
  ;; Evaluate form and send result back
  ...)
```

### 4.3 sly-stickers (Live Annotations)

**Purpose:** Mark expressions and see their values recorded as code executes.

**Files:**
- `contrib/sly-stickers.el` (1,358 lines)
- `contrib/slynk-stickers.lisp` (~600 lines)

**Key features:**
- Non-invasive code instrumentation
- Value recording with timestamps
- Zombie sticker detection
- Interactive sticker management

**How it works:**
1. User places sticker on expression
2. Code is macro-expanded to record values
3. Values streamed to Emacs
4. Displayed inline or in separate buffer

**Elisp highlights:**
```elisp
(define-sly-contrib sly-stickers
  "Live code annotations for debugging."
  (:slynk-dependencies slynk/stickers)
  (:on-load
   (add-hook 'sly-compilation-finished-hook 'sly-stickers--compile-region-aware))
  (:on-unload
   (remove-hook 'sly-compilation-finished-hook 'sly-stickers--compile-region-aware)))

;; Sticker overlay management
(defun sly-stickers--stickers-in (beg end)
  "Return stickers overlapping BEG to END."
  ...)
```

### 4.4 sly-trace-dialog (Interactive Tracing)

**Purpose:** Interactive trace browser with hierarchical display.

**Files:**
- `contrib/sly-trace-dialog.el` (743 lines)
- `contrib/slynk-trace-dialog.lisp` (~300 lines)

**Key features:**
- Tree-structured trace display
- Entry/exit value inspection
- Interactive navigation
- Trace recording/playback

### 4.5 sly-autodoc (Automatic Documentation)

**Purpose:** Display function signatures in echo area.

**Files:**
- `contrib/sly-autodoc.el` (188 lines)
- Uses `slynk/arglists` module

**How it works:**
```elisp
(define-sly-contrib sly-autodoc
  "Display argument lists in echo area."
  (:slynk-dependencies slynk/arglists)
  (:on-load
   (add-hook 'sly-editing-mode-hook 'sly-autodoc-mode))
  (:on-unload
   (remove-hook 'sly-editing-mode-hook 'sly-autodoc-mode)))

;; Uses eldoc for display
(defun sly-autodoc--eldoc-function ()
  "Query SLYNK for arglist at point."
  (sly-eval-async `(slynk:autodoc ',(sly-parse-context))
    #'sly-autodoc--display))
```

### 4.6 sly-fancy-inspector (Enhanced Inspector)

**Purpose:** Rich object inspection with CLOS awareness.

**Files:**
- `contrib/sly-fancy-inspector.el` (22 lines - just loads SLYNK)
- `contrib/slynk-fancy-inspector.lisp` (1,300+ lines)

### 4.7 sly-package-fu (Package Utilities)

**Purpose:** Package manipulation commands.

**Files:**
- `contrib/sly-package-fu.el` (448 lines)
- `contrib/slynk-package-fu.lisp` (~100 lines)

**Features:**
- Export symbol at point
- Unexport symbol
- Import symbol
- Add to defpackage

### 4.8 sly-fontifying-fu (Enhanced Highlighting)

**Purpose:** Additional syntax highlighting.

**File:** `contrib/sly-fontifying-fu.el` (206 lines)

**Features:**
- Highlight `with-*`, `define-*` forms
- Dim reader-conditionals for other implementations
- Enhanced keyword highlighting

### 4.9 sly-profiler (Profiling UI)

**Purpose:** Function timing/profiling interface.

**Files:**
- `contrib/sly-profiler.el` (155 lines)
- `contrib/slynk-profiler.lisp` (~200 lines)

### 4.10 sly-tramp (Remote Lisp)

**Purpose:** Support for remote Lisp via TRAMP.

**File:** `contrib/sly-tramp.el` (123 lines)

**Features:**
- Path translation between local and remote
- Per-host configuration
- SSH tunnel integration

### 4.11 sly-retro (SLIME Compatibility)

**Purpose:** Allow SLIME clients to connect to SLYNK.

**Files:**
- `contrib/sly-retro.el` (22 lines)
- `contrib/slynk-retro.lisp` (45 lines)

Translates SWANK protocol to SLYNK protocol.

### 4.12 sly-scratch (Scratch Buffer)

**Purpose:** Lisp-mode scratch buffer.

**File:** `contrib/sly-scratch.el` (45 lines)

### 4.13 sly-indentation (Dynamic Indentation)

**Purpose:** Load indentation rules from Lisp.

**Files:**
- `contrib/sly-indentation.el` (31 lines)
- `contrib/slynk-indentation.lisp` (~150 lines)

### 4.14 sly-fancy-trace (Enhanced Tracing)

**Purpose:** Trace local functions and methods.

**File:** `contrib/sly-fancy-trace.el` (68 lines)

---

## 5. Contrib Patterns

### 5.1 Pattern: Simple UI Enhancement

For contribs that only add Emacs-side features:

```elisp
(define-sly-contrib sly-my-feature
  "Add MY-FEATURE to SLY."
  (:on-load
   (define-key sly-mode-map (kbd "C-c M") 'sly-my-command)
   (add-hook 'sly-mode-hook 'sly-my-minor-mode))
  (:on-unload
   (define-key sly-mode-map (kbd "C-c M") nil)
   (remove-hook 'sly-mode-hook 'sly-my-minor-mode)))

(define-minor-mode sly-my-minor-mode
  "Minor mode for MY-FEATURE."
  :lighter " my"
  :keymap (make-sparse-keymap)
  ...)

(defun sly-my-command ()
  "Do the thing."
  (interactive)
  ...)
```

### 5.2 Pattern: Server Integration

For contribs that need SLYNK-side code:

**Elisp side:**
```elisp
(define-sly-contrib sly-server-feature
  "Feature requiring server-side code."
  (:slynk-dependencies slynk/server-feature)
  (:on-load
   (add-hook 'sly-connected-hook 'sly-server-feature-init))
  (:on-unload
   (remove-hook 'sly-connected-hook 'sly-server-feature-init)))

(defun sly-server-feature-init ()
  "Initialize server-feature on connection."
  (sly-eval-async '(slynk:server-feature-setup)
    (lambda (result)
      (message "Server feature ready: %s" result))))

(defun sly-server-feature-command ()
  "Call server-side function."
  (interactive)
  (sly-eval-async '(slynk:server-feature-do-thing)
    #'sly-server-feature-handle-result))
```

**SLYNK side:**
```lisp
(defpackage :slynk/server-feature
  (:use :cl :slynk)
  (:export #:server-feature-setup
           #:server-feature-do-thing))

(in-package :slynk/server-feature)

(defslyfun server-feature-setup ()
  "Initialize server-side feature."
  (setf *feature-state* (make-feature-state))
  :ready)

(defslyfun server-feature-do-thing ()
  "Perform the feature action."
  (do-the-thing *feature-state*))
```

**ASDF registration (slynk.asd):**
```lisp
(asdf:defsystem :slynk/server-feature
  :depends-on (:slynk)
  :components ((:file "contrib/slynk-server-feature")))
```

### 5.3 Pattern: Channel-Based Communication

For streaming data or stateful interactions:

**Elisp side:**
```elisp
(define-sly-contrib sly-streaming
  "Stream data from Lisp."
  (:slynk-dependencies slynk/streaming)
  (:on-load ...))

(defun sly-streaming-start ()
  "Start streaming data."
  (interactive)
  (let ((channel (sly-make-channel #'sly-streaming-handler)))
    (sly-eval-async `(slynk:streaming-start ,(sly-channel.id channel))
      #'sly-streaming-started)))

(defun sly-streaming-handler (method &rest args)
  "Handle messages from streaming channel."
  (pcase method
    (:data (apply #'sly-streaming-receive-data args))
    (:complete (sly-streaming-complete))
    (_ (message "Unknown method: %s" method))))
```

**SLYNK side:**
```lisp
(defclass streaming-channel (channel)
  ((data-source :initarg :data-source)))

(defslyfun streaming-start (channel-id)
  "Start streaming on CHANNEL-ID."
  (let ((channel (find-channel channel-id)))
    (spawn (lambda ()
             (loop for data = (get-next-data)
                   while data
                   do (channel-send channel :data data))
             (channel-send channel :complete)))))
```

### 5.4 Pattern: Dependency Bundling

For grouping related contribs:

```elisp
(define-sly-contrib sly-my-bundle
  "Bundle of related features."
  (:sly-dependencies sly-feature-a
                     sly-feature-b
                     sly-feature-c))
```

---

## 6. SLYNK Module System

### 6.1 Module Names

SLYNK modules use forward-slash notation:
- `slynk/mrepl` → loads `slynk-mrepl.lisp`
- `slynk/stickers` → loads `slynk-stickers.lisp`

### 6.2 ASDF System Definition

All SLYNK modules are defined in `slynk/slynk.asd`:

```lisp
(asdf:defsystem :slynk/mrepl
  :depends-on (:slynk)
  :components ((:file "contrib/slynk-mrepl")))

(asdf:defsystem :slynk/stickers
  :depends-on (:slynk)
  :components ((:file "contrib/slynk-stickers")))

;; etc.
```

### 6.3 Require Hook

Modules can register for load notifications:

```lisp
;; slynk.lisp:2988
(defvar *slynk-require-hook* '()
  "Functions called after modules are loaded.
Each function receives the list of loaded module names.")
```

---

## 7. Creating a New Contrib

### 7.1 Step-by-Step Guide

**1. Create Elisp file:**

```elisp
;; contrib/sly-my-feature.el

;;; Commentary:
;;
;; Adds MY-FEATURE to SLY.
;;
;;; Code:

(require 'sly)

(define-sly-contrib sly-my-feature
  "Short description of my feature."
  (:authors "Your Name <email@example.com>")
  (:license "GPL")
  (:slynk-dependencies slynk/my-feature)  ; if needed
  (:on-load
   (sly-my-feature-mode 1))
  (:on-unload
   (sly-my-feature-mode -1)))

(define-minor-mode sly-my-feature-mode
  "Minor mode for my feature."
  :global t
  :lighter " MyF"
  (if sly-my-feature-mode
      (sly-my-feature-enable)
    (sly-my-feature-disable)))

(defun sly-my-feature-enable ()
  "Set up my feature."
  (add-hook 'sly-mode-hook 'sly-my-feature-setup))

(defun sly-my-feature-disable ()
  "Tear down my feature."
  (remove-hook 'sly-mode-hook 'sly-my-feature-setup))

(defun sly-my-feature-setup ()
  "Per-buffer setup."
  ...)

(provide 'sly-my-feature)
;;; sly-my-feature.el ends here
```

**2. Create SLYNK file (if needed):**

```lisp
;; contrib/slynk-my-feature.lisp

(defpackage :slynk/my-feature
  (:use :cl :slynk)
  (:export #:my-feature-info
           #:my-feature-action))

(in-package :slynk/my-feature)

(defslyfun my-feature-info ()
  "Return information about my feature."
  `(:version "1.0"
    :status :ready))

(defslyfun my-feature-action (arg)
  "Perform my feature action."
  (do-something-with arg))
```

**3. Register ASDF system:**

Add to `slynk/slynk.asd`:

```lisp
(asdf:defsystem :slynk/my-feature
  :depends-on (:slynk)
  :components ((:file "contrib/slynk-my-feature")))
```

**4. Create tests:**

```elisp
;; test/sly-my-feature-tests.el

(require 'sly-my-feature)
(require 'sly-tests)

(def-sly-test my-feature-basic ()
  "Test basic my-feature functionality."
  (sly-check ("Feature responds"
              (sly-eval '(slynk:my-feature-info))))
  (sly-check ("Action works"
              (sly-eval '(slynk:my-feature-action 42)))))

(provide 'sly-my-feature-tests)
```

### 7.2 Best Practices

**DO:**
- Use `sly-eval-async` for non-blocking operations
- Clean up hooks/keybindings in `:on-unload`
- Prefix all functions with `sly-<contrib-name>-`
- Export SLYNK functions with `defslyfun`
- Handle connection loss gracefully

**DON'T:**
- Block with synchronous eval in init code
- Leave dangling hooks on unload
- Assume a connection exists
- Pollute global namespace

---

## 8. Testing Contribs

### 8.1 Test Framework

SLY uses ERT (Emacs Regression Testing) with extensions:

```elisp
(require 'sly-tests)

(def-sly-test test-name (arg1 arg2)
  "Docstring for test."
  '((arg1-val-1 arg2-val-1)    ; Test case 1
    (arg1-val-2 arg2-val-2))   ; Test case 2
  (sly-check ("Check description"
              (some-predicate arg1 arg2))))
```

### 8.2 Running Tests

```bash
# Run all tests
make check

# Run specific contrib tests
make check-mrepl
make check-stickers
```

---

## 9. Contrib Configuration Reference

### 9.1 User Variables

Each contrib may define customizable variables:

```elisp
(defcustom sly-mrepl-pop-on-connect t
  "Pop to REPL buffer on connection."
  :type 'boolean
  :group 'sly-mrepl)

(defcustom sly-stickers-max-recorded-values 100
  "Maximum sticker values to keep."
  :type 'integer
  :group 'sly-stickers)
```

### 9.2 Selecting Contribs

In user's init file:

```elisp
;; Load default "fancy" contribs
(sly-setup '(sly-fancy))

;; Or select specific contribs
(sly-setup '(sly-mrepl sly-stickers sly-autodoc))

;; Or add to existing
(setq sly-contribs '(sly-fancy sly-my-custom-feature))
(sly-setup sly-contribs)
```

---

## 10. Contrib Directory Summary

| Contrib | Elisp Lines | SLYNK Lines | Purpose |
|---------|-------------|-------------|---------|
| sly-fancy | 22 | - | Bundle of common contribs |
| sly-mrepl | 1,568 | ~500 | Multiple REPLs |
| sly-stickers | 1,358 | ~600 | Live annotations |
| sly-trace-dialog | 743 | ~300 | Interactive tracing |
| sly-package-fu | 448 | ~100 | Package utilities |
| sly-fontifying-fu | 206 | - | Enhanced highlighting |
| sly-autodoc | 188 | (arglists) | Auto documentation |
| sly-profiler | 155 | ~200 | Profiling UI |
| sly-tramp | 123 | - | Remote Lisp |
| sly-fancy-trace | 68 | - | Enhanced tracing |
| sly-scratch | 45 | - | Scratch buffer |
| sly-indentation | 31 | ~150 | Dynamic indentation |
| sly-fancy-inspector | 22 | ~1,300 | Rich inspection |
| sly-retro | 22 | 45 | SLIME compat |

---

*Document generated from SLY source code analysis. Last updated: 2025.*
