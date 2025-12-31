# SLY Architecture

This document provides a comprehensive analysis of SLY's architecture, covering both the Emacs client and the SLYNK server components.

## Overview

SLY (Sylvester the Cat's Common Lisp IDE) is a fork of SLIME that provides an integrated development environment for Common Lisp within Emacs. The architecture follows a client-server model:

```
┌─────────────────────────────────────────────────────────────────┐
│                          EMACS                                   │
│  ┌────────────────────────────────────────────────────────────┐ │
│  │                      SLY (Elisp)                           │ │
│  │  ┌──────────┐ ┌──────────┐ ┌──────────┐ ┌───────────────┐  │ │
│  │  │ sly.el   │ │ lib/*.el │ │ contribs │ │ Mode Buffers  │  │ │
│  │  │ (core)   │ │(utilities)│ │ sly-*.el │ │ (SLY-DB, etc) │  │ │
│  │  └────┬─────┘ └──────────┘ └──────────┘ └───────────────┘  │ │
│  │       │                                                     │ │
│  │       │ TCP/IP (port 4005)                                  │ │
│  └───────┼─────────────────────────────────────────────────────┘ │
└──────────┼──────────────────────────────────────────────────────┘
           │
           │ SLYNK Protocol (S-expressions over TCP)
           │
┌──────────┼──────────────────────────────────────────────────────┐
│          ▼                                                       │
│  ┌─────────────────────────────────────────────────────────────┐ │
│  │                    SLYNK (Common Lisp)                      │ │
│  │  ┌──────────────┐ ┌─────────────────┐ ┌──────────────────┐  │ │
│  │  │ slynk.lisp   │ │ slynk-backend   │ │ Backend Impl     │  │ │
│  │  │ (core)       │ │ (abstraction)   │ │ (sbcl.lisp, etc) │  │ │
│  │  └──────────────┘ └─────────────────┘ └──────────────────┘  │ │
│  │                                                              │ │
│  │  ┌──────────────────────────────────────────────────────┐   │ │
│  │  │              Lisp Implementation Runtime              │   │ │
│  │  │               (SBCL, CCL, ABCL, etc.)                │   │ │
│  │  └──────────────────────────────────────────────────────┘   │ │
│  └─────────────────────────────────────────────────────────────┘ │
│                       Common Lisp Process                        │
└──────────────────────────────────────────────────────────────────┘
```

---

## 1. Directory Structure

```
sly/
├── sly.el                 # Main entry point (7,511 lines)
├── sly-autoloads.el       # Autoload definitions
├── Makefile               # Build automation
│
├── lib/                   # Core Elisp utilities
│   ├── sly-common.el      # Shared utilities (76 lines)
│   ├── sly-messages.el    # Echo area messaging (137 lines)
│   ├── sly-buttons.el     # Interactive button UI (355 lines)
│   ├── sly-completion.el  # Completion framework (806 lines)
│   ├── sly-parse.el       # S-expression parsing (355 lines)
│   ├── sly-cl-indent.el   # CL indentation (1,885 lines)
│   ├── sly-tests.el       # Test framework (1,545 lines)
│   └── hyperspec.el       # HyperSpec lookup (2,630 lines)
│
├── slynk/                 # Common Lisp server
│   ├── slynk.lisp         # Main server (4,236 lines)
│   ├── slynk-backend.lisp # Backend interface (1,400+ lines)
│   ├── slynk-rpc.lisp     # Wire protocol (212 lines)
│   ├── slynk-match.lisp   # Pattern matching (244 lines)
│   ├── slynk-completion.lisp  # Completion (565 lines)
│   ├── slynk-apropos.lisp # Symbol search (240 lines)
│   ├── slynk-gray.lisp    # Gray streams (240 lines)
│   ├── slynk-source-path-parser.lisp  # Source parsing (246 lines)
│   ├── slynk-source-file-cache.lisp   # File caching (134 lines)
│   ├── xref.lisp          # Cross-references (2,904 lines)
│   ├── metering.lisp      # Profiling support (1,530 lines)
│   ├── slynk.asd          # ASDF system definition
│   ├── slynk-loader.lisp  # Legacy loader (380+ lines)
│   └── backend/           # Implementation-specific code
│       ├── sbcl.lisp      # SBCL backend (2,024 lines)
│       ├── ccl.lisp       # Clozure CL (874 lines)
│       ├── abcl.lisp      # Armed Bear CL (1,530 lines)
│       ├── cmucl.lisp     # CMU CL (2,483 lines)
│       ├── allegro.lisp   # Allegro CL (1,115 lines)
│       ├── lispworks.lisp # LispWorks (1,033 lines)
│       ├── ecl.lisp       # ECL (1,093 lines)
│       ├── clisp.lisp     # CLISP (921 lines)
│       ├── clasp.lisp     # Clasp (737 lines)
│       ├── scl.lisp       # Scieneer CL (1,726 lines)
│       ├── mkcl.lisp      # ManKai CL (934 lines)
│       └── corman.lisp    # Corman Lisp (583 lines)
│
├── contrib/               # Optional features
│   ├── sly-mrepl.el       # Multiple REPLs (1,568 lines)
│   ├── sly-stickers.el    # Live annotations (1,358 lines)
│   ├── sly-trace-dialog.el    # Trace browser (743 lines)
│   ├── slynk-mrepl.lisp   # MREPL server-side (500+ lines)
│   ├── slynk-stickers.lisp    # Sticker server (600+ lines)
│   └── ... (more contribs)
│
├── test/                  # Test suite
│   ├── sly-*-tests.el     # Feature tests
│   └── sly-cl-indent-test.txt  # Indentation tests
│
└── doc/                   # Documentation
    ├── sly.texi           # Texinfo manual
    └── images/            # Screenshots
```

---

## 2. Emacs Side Architecture

### 2.1 Core Module: sly.el

The main `sly.el` file (7,511 lines) is organized into these major sections:

| Section | Lines | Purpose |
|---------|-------|---------|
| Header & Dependencies | 1-110 | Package declaration, requires |
| Customization | 111-400 | User-facing options |
| Keymaps & Modes | 401-700 | Mode definitions, keybindings |
| Starting SLY | 701-1000 | Process startup, initialization |
| Networking | 1001-1750 | TCP connections, protocol |
| Connections | 1751-2250 | Connection state management |
| RPC Interface | 2251-2700 | Remote procedure calls |
| Compilation | 2701-3500 | Compiler notes, warnings |
| Evaluation | 3501-4000 | Interactive evaluation |
| Definition Navigation | 4001-4500 | Edit-definition, XREF |
| Documentation | 4501-5000 | Docstrings, describe |
| Macroexpansion | 5001-5300 | Macro expansion UI |
| SLY-DB (Debugger) | 5301-6400 | Debugger interface |
| Inspector | 6401-6900 | Object inspection |
| Contrib System | 6901-7100 | Contrib loading |
| Utilities | 7101-7511 | Helper functions |

### 2.2 Major Subsystems

#### 2.2.1 Connection Management

```elisp
;; Connection structure (sly.el:1751-1850)
(cl-defstruct (sly-connection (:conc-name sly-connection.))
  process                  ; Network process
  id                       ; Unique connection ID
  name                     ; Display name
  coding-system            ; Character encoding
  server-implementation    ; Lisp implementation info
  pid                      ; Remote process ID
  rex-continuations        ; Pending RPC callbacks
  channels                 ; Active channels
  ...)
```

**Key variables:**
- `sly-net-processes` - List of active network processes
- `sly-default-connection` - Current default connection
- `sly-buffer-connection` - Buffer-local connection

**Connection lifecycle:**
1. `sly` / `sly-connect` - Initiate connection
2. `sly-net-connect` - Establish TCP socket
3. `sly-setup-connection` - Configure connection
4. `sly-init-connection` - Exchange capabilities
5. `sly-net-close` - Terminate connection

#### 2.2.2 RPC Layer

```elisp
;; Synchronous evaluation
(defun sly-eval (sexp &optional package)
  "Evaluate SEXP in Lisp and return the result."
  (let ((result (sly-eval-async sexp #'identity package)))
    (while (not result)
      (accept-process-output))
    result))

;; Asynchronous evaluation
(defun sly-eval-async (sexp &optional cont package)
  "Evaluate SEXP in Lisp. Call CONT with result when done."
  (sly-dispatch-event
   `(:emacs-rex ,sexp ,(or package (sly-current-package))
                ,(or (sly-current-thread) t)
                ,cont)))
```

#### 2.2.3 SLY-DB (Debugger)

The debugger is a major-mode derived from `special-mode`:

```elisp
(define-derived-mode sly-db-mode special-mode "SLY-DB"
  "Mode for debugging Common Lisp conditions."
  (setq-local truncate-lines t)
  (setq-local sly-db-level nil)
  ...)
```

**Buffer structure:**
```
Condition: DIVISION-BY-ZERO
  [Condition details...]

Restarts:
  0: [ABORT] Return to REPL
  1: [CONTINUE] Use a value
  ...

Backtrace:
  0: (/ 1 0)
  1: (EVAL (/ 1 0))
  ...
```

**Key commands:**
- `q` - Quit (abort)
- `c` - Continue
- `0-9` - Invoke restart by number
- `v` - View frame source
- `e` - Eval in frame
- `i` - Inspect in frame

#### 2.2.4 Inspector

Object inspection UI:

```elisp
(define-derived-mode sly-inspector-mode special-mode "SLY-Inspector"
  "Mode for inspecting Lisp objects."
  ...)
```

**Buffer structure:**
```
#<HASH-TABLE :TEST EQL :COUNT 3>
--------------------
Type: HASH-TABLE
Count: 3
Size: 16
Rehash-size: 1.5
Rehash-threshold: 1.0

[Contents]
  :A -> 1
  :B -> 2
  :C -> 3
```

### 2.3 Library Modules (lib/)

#### 2.3.1 sly-completion.el

Provides completion backend supporting multiple frontends:

```elisp
;; Completion table function
(defun sly--completion-function (string pred action)
  (pcase action
    ('metadata ...)       ; Metadata about completions
    ('t ...)              ; All completions
    ('nil ...)            ; Try-completion
    ((pred functionp) ...)  ; Lambda action
    ...))
```

**Supported frontends:**
- Vanilla Emacs completion
- Company-mode
- Helm
- Ivy

#### 2.3.2 sly-buttons.el

Creates interactive buttons in SLY buffers:

```elisp
(define-button-type 'sly-part
  'action 'sly-button-action
  'mouse-action 'sly-button-action
  'sly-button-search-id nil
  ...)
```

Used for clickable objects in:
- Inspector values
- REPL output
- Debugger frames

#### 2.3.3 sly-parse.el

S-expression aware parsing:

```elisp
(defun sly-parse-form-until (position)
  "Parse form from buffer start to POSITION."
  ...)

(defun sly-parse-context ()
  "Return context around point for autodoc."
  ...)
```

#### 2.3.4 sly-cl-indent.el

Enhanced Common Lisp indentation (forked from Emacs's cl-indent.el):

- Loop clause alignment
- Backquote handling
- Custom indentation specs per symbol
- Package-qualified symbol support

---

## 3. SLYNK Side Architecture

### 3.1 Package Structure

```lisp
;; Core packages
:slynk              ; Main server package
:slynk-backend      ; Implementation abstraction
:slynk-rpc          ; Wire protocol
:slynk-mop          ; CLOS introspection portability
:slynk-match        ; Pattern matching for completion
:slynk-completion   ; Completion algorithms
:slynk-apropos      ; Symbol search
:slynk-io-package   ; Clean namespace for protocol I/O
```

### 3.2 Core Server (slynk.lisp)

#### 3.2.1 Server Startup

```lisp
(defun create-server (&key (port default-server-port)
                           (style *communication-style*)
                           (dont-close nil))
  "Create a SLYNK server listening on PORT."
  (setup-server port
                (lambda (socket)
                  (accept-authenticated-connection socket))
                style
                dont-close))
```

**Communication styles:**
| Style | Threading Model | Use Case |
|-------|-----------------|----------|
| `:spawn` | Full multithreading | Default for threaded Lisps |
| `:sigio` | Signal-based I/O | Single-threaded alternative |
| `:fd-handler` | FD handler callbacks | Event loop integration |
| `nil` | Polling | Simplest, most portable |

#### 3.2.2 Connection Handling

```lisp
(defstruct (connection (:constructor %make-connection))
  socket                    ; TCP socket
  socket-io                 ; I/O stream
  (channel-counter 0)       ; Channel ID generator
  (channels '())            ; Active channels
  (listeners '())           ; Event listeners
  (inspectors '())          ; Inspector state
  indentation-cache         ; Symbol indentation info
  communication-style)      ; Threading model

(defstruct (multithreaded-connection (:include connection))
  reader-thread            ; Network reader
  control-thread           ; Event dispatcher
  auto-flush-thread        ; Output flusher
  (active-threads '()))    ; Worker threads

(defstruct (singlethreaded-connection (:include connection))
  (event-queue '())        ; Pending events
  (events-enqueued 0))     ; Event counter
```

#### 3.2.3 Event Loop

**Multithreaded:**
```lisp
(defun read-loop (connection)
  "Read messages from socket, send to control thread."
  (let ((input-stream (connection-socket-io connection)))
    (with-slynk-error-handler (connection)
      (loop (send control-thread (decode-message input-stream))))))

(defun dispatch-loop (connection)
  "Receive events from reader, dispatch to handlers."
  (let ((*emacs-connection* connection))
    (with-panic-handler (connection)
      (loop (dispatch-event connection (receive))))))
```

**Event dispatch:**
```lisp
(defun dispatch-event (connection event)
  (destructure-case event
    ((:emacs-rex form package thread-id id &rest extra)
     (spawn-worker-for-request connection form package thread-id id extra))
    ((:return thread &rest args)
     (encode-message `(:return ,@args) (current-socket-io)))
    ((:emacs-interrupt thread-id)
     (interrupt-worker connection thread-id))
    ...))
```

#### 3.2.4 RPC Function Definition

```lisp
(defmacro defslyfun (name arglist &body body)
  "Define a function callable from Emacs via RPC."
  `(progn
     (defun ,name ,arglist ,@body)
     (export ',name :slynk)))
```

**Examples:**
```lisp
(defslyfun connection-info ()
  "Return version and implementation info."
  `(:pid ,(getpid)
    :lisp-implementation (:type ,(lisp-implementation-type)
                          :version ,(lisp-implementation-version))
    :machine (:type ,(machine-type))
    :features ,(features-for-emacs)
    :package ,(package-name *package*)
    :version ,*slynk-wire-protocol-version*))

(defslyfun interactive-eval (string)
  "Evaluate STRING and return result as string."
  (with-buffer-syntax ()
    (with-retry-restart (:msg "Retry SLY evaluation.")
      (let ((values (multiple-value-list (eval (from-string string)))))
        (format nil "~{~S~^~%~}" values)))))
```

### 3.3 Backend Abstraction (slynk-backend.lisp)

The backend defines generic functions that must be specialized per Lisp implementation:

```lisp
(defpackage slynk-backend
  (:use :common-lisp)
  (:export
   ;; Compilation
   #:compile-string-for-emacs
   #:compile-file-for-emacs

   ;; Debugging
   #:call-with-debugger-hook
   #:compute-backtrace
   #:frame-locals
   #:frame-source-location
   #:invoke-debugger

   ;; Introspection
   #:arglist
   #:function-name
   #:macroexpand-all
   #:describe-symbol-for-emacs

   ;; Source location
   #:find-definitions
   #:buffer-first-change
   ...))

(defmacro defimplementation (name args &body body)
  "Define implementation-specific behavior."
  `(defmethod ,name ,args ,@body))
```

### 3.4 Backend Implementations

Each backend file specializes the generic functions:

#### 3.4.1 SBCL Backend (sbcl.lisp)

```lisp
(defimplementation compute-backtrace (start end)
  "Return backtrace frames from START to END."
  (loop for frame in (sb-debug:list-backtrace)
        for i from 0
        when (and (>= i start) (or (null end) (< i end)))
        collect (make-frame :number i
                           :description (frame-to-string frame)
                           :restartable (restartable-frame-p frame))))

(defimplementation frame-source-location (index)
  "Return source location for frame INDEX."
  (let* ((frame (nth index (sb-debug:list-backtrace)))
         (debug-fun (sb-di:frame-debug-fun frame))
         (code-location (sb-di:frame-code-location frame)))
    (cond ((sb-di:debug-source-namestring
            (sb-di:code-location-debug-source code-location))
           => (lambda (file)
                (make-location :file file
                              :position (safe-source-location-for-emacs code-location))))
          (t nil))))
```

#### 3.4.2 Implementation Feature Matrix

| Feature | SBCL | CCL | ABCL | ECL | CLISP |
|---------|------|-----|------|-----|-------|
| Threading | Full | Full | Full | Full | Limited |
| Debugging | Full | Full | Basic | Basic | Basic |
| Profiling | Full | Full | Basic | Basic | None |
| CLOS Inspect | Full | Full | Full | Basic | Basic |
| Source Locs | Full | Full | Basic | Basic | Basic |

### 3.5 Cross-Reference System (xref.lisp)

Provides "who calls", "who references", etc.:

```lisp
(defun who-calls (function-name)
  "Return list of functions that call FUNCTION-NAME."
  ...)

(defun who-references (variable-name)
  "Return list of functions that reference VARIABLE-NAME."
  ...)

(defun who-binds (variable-name)
  "Return list of functions that bind VARIABLE-NAME."
  ...)

(defun who-sets (variable-name)
  "Return list of functions that set VARIABLE-NAME."
  ...)

(defun who-macroexpands (macro-name)
  "Return list of functions that expand MACRO-NAME."
  ...)

(defun who-specializes (class-name)
  "Return methods specializing on CLASS-NAME."
  ...)
```

---

## 4. Data Flow

### 4.1 Evaluation Flow

```
┌────────────────────────────────────────────────────────────────┐
│                         EMACS                                   │
│                                                                │
│  User types: (+ 1 2)                                           │
│       │                                                        │
│       ▼                                                        │
│  sly-eval-last-expression                                      │
│       │                                                        │
│       ▼                                                        │
│  sly-eval-async '(slynk:interactive-eval "(+ 1 2)")           │
│       │                                                        │
│       ▼                                                        │
│  sly-dispatch-event (:emacs-rex ...)                          │
│       │                                                        │
│       ▼                                                        │
│  sly-net-send  →  TCP  →                                       │
└───────────────────────┼────────────────────────────────────────┘
                        │
                        ▼
┌───────────────────────────────────────────────────────────────┐
│                        SLYNK                                   │
│                                                                │
│  decode-message                                                │
│       │                                                        │
│       ▼                                                        │
│  dispatch-event (:emacs-rex ...)                              │
│       │                                                        │
│       ▼                                                        │
│  spawn-worker-thread                                           │
│       │                                                        │
│       ▼                                                        │
│  slynk:interactive-eval "(+ 1 2)"                             │
│       │                                                        │
│       ▼                                                        │
│  (eval (from-string "(+ 1 2)"))  →  3                         │
│       │                                                        │
│       ▼                                                        │
│  format result as string "3"                                   │
│       │                                                        │
│       ▼                                                        │
│  send-to-emacs (:return (:ok "3") request-id)                 │
│       │                                                        │
│       ▼                                                        │
│  encode-message  →  TCP  →                                     │
└───────────────────────┼───────────────────────────────────────┘
                        │
                        ▼
┌───────────────────────────────────────────────────────────────┐
│                         EMACS                                  │
│                                                                │
│  sly-net-filter (receive data)                                │
│       │                                                        │
│       ▼                                                        │
│  sly-dispatch-event (:return ...)                             │
│       │                                                        │
│       ▼                                                        │
│  find continuation by request-id                               │
│       │                                                        │
│       ▼                                                        │
│  invoke callback with "3"                                      │
│       │                                                        │
│       ▼                                                        │
│  Display "3" in echo area                                      │
└───────────────────────────────────────────────────────────────┘
```

### 4.2 Debugger Flow

```
┌─────────────────────────────────────────────────────────────┐
│  SLYNK: Error occurs during evaluation                      │
│                                                             │
│  invoke-slynk-debugger                                      │
│       │                                                     │
│       ▼                                                     │
│  Compute condition info, restarts, backtrace               │
│       │                                                     │
│       ▼                                                     │
│  send-to-emacs (:debug thread level condition restarts ...) │
│  send-to-emacs (:debug-activate thread level)              │
│       │                                                     │
│       ▼                                                     │
│  Wait for debug commands...                                 │
└──────────────────────────┼──────────────────────────────────┘
                           │
                           ▼
┌─────────────────────────────────────────────────────────────┐
│  EMACS: Receive debug events                                │
│                                                             │
│  sly-dispatch-event (:debug ...)                           │
│       │                                                     │
│       ▼                                                     │
│  Create *sly-db* buffer                                    │
│  Display condition, restarts, backtrace                    │
│       │                                                     │
│  User presses '0' (invoke restart 0)                       │
│       │                                                     │
│       ▼                                                     │
│  sly-db-invoke-restart 0                                   │
│       │                                                     │
│       ▼                                                     │
│  (:emacs-rex (slynk:invoke-nth-restart-for-emacs ...) ...)│
└──────────────────────────┼──────────────────────────────────┘
                           │
                           ▼
┌─────────────────────────────────────────────────────────────┐
│  SLYNK: Process restart command                            │
│                                                             │
│  invoke-nth-restart-for-emacs                              │
│       │                                                     │
│       ▼                                                     │
│  invoke-restart (nth restarts n)                           │
│       │                                                     │
│       ▼                                                     │
│  Control returns to appropriate point                       │
│       │                                                     │
│       ▼                                                     │
│  send-to-emacs (:debug-return thread level nil)            │
└─────────────────────────────────────────────────────────────┘
```

---

## 5. Initialization Sequence

### 5.1 Emacs Side Startup

```elisp
;; User invokes M-x sly
(defun sly (&optional command coding-system)
  (interactive)
  (let ((inferior-lisp-program (or command inferior-lisp-program)))
    (sly-start :program inferior-lisp-program)))

(defun sly-start (&key program program-args env directory coding-system)
  ;; 1. Setup contrib system
  (sly-setup sly-contribs)

  ;; 2. Start inferior Lisp process
  (let ((proc (sly-inferior-lisp-start program program-args env directory)))

    ;; 3. Inject SLYNK loading code
    (sly-inferior-lisp-inject-loader proc coding-system)

    ;; 4. Connect when server ready
    (sly-connect-when-server-ready proc)))
```

### 5.2 SLYNK Loading

SLY injects this code into the Lisp process:

```lisp
;; Method 1: ASDF (preferred)
(asdf:load-system :slynk)
(slynk:create-server :port 4005)

;; Method 2: Legacy loader
(load "~/.emacs.d/elpa/sly/slynk/slynk-loader.lisp")
(slynk-loader:init)
(slynk:create-server :port 4005)
```

### 5.3 Connection Establishment

```elisp
(defun sly-net-connect (host port)
  ;; 1. Open TCP connection
  (let ((connection (open-network-stream "sly" nil host port)))

    ;; 2. Setup filter for incoming data
    (set-process-filter connection 'sly-net-filter)

    ;; 3. Send secret if configured
    (sly-send-secret connection)

    ;; 4. Return connection
    connection))

(defun sly-setup-connection (connection)
  ;; 1. Request connection info
  (sly-eval-async '(slynk:connection-info)
    (lambda (info)
      ;; 2. Store server capabilities
      (setf (sly-connection.server-implementation connection)
            (plist-get info :lisp-implementation))

      ;; 3. Load required SLYNK modules
      (sly-contrib--load-slynk-dependencies)

      ;; 4. Initialize contribs
      (run-hooks 'sly-connected-hook))))
```

---

## 6. Buffer Management

### 6.1 Buffer Types

| Buffer Pattern | Mode | Purpose |
|----------------|------|---------|
| `*sly-mrepl for ...*` | `sly-mrepl-mode` | REPL interaction |
| `*sly-db ...*` | `sly-db-mode` | Debugger |
| `*sly-inspector*` | `sly-inspector-mode` | Object inspection |
| `*sly-xref*` | `sly-xref-mode` | Cross-references |
| `*sly-description*` | `sly-description-mode` | Documentation |
| `*sly-macroexpansion*` | `sly-macroexpansion-mode` | Macro expansion |
| `*sly-trace-dialog*` | `sly-trace-dialog-mode` | Trace browser |

### 6.2 Buffer-Connection Association

```elisp
;; Each buffer tracks its connection
(defvar-local sly-buffer-connection nil
  "Network connection for this buffer.")

(defun sly-connection ()
  "Return connection for current buffer."
  (or sly-buffer-connection
      sly-default-connection
      (error "No SLY connection")))
```

---

## 7. Key Interfaces

### 7.1 Public Elisp API

**Connection:**
```elisp
(sly)                           ; Start Lisp and connect
(sly-connect host port)         ; Connect to running server
(sly-disconnect)                ; Close connection
(sly-restart-inferior-lisp)     ; Restart Lisp process
```

**Evaluation:**
```elisp
(sly-eval sexp &optional package)           ; Sync eval
(sly-eval-async sexp cont &optional pkg)    ; Async eval
(sly-interactive-eval string)               ; Eval with output
```

**Navigation:**
```elisp
(sly-edit-definition name)      ; Jump to definition
(sly-pop-find-definition-stack) ; Return from definition
(sly-who-calls name)            ; Find callers
```

**Documentation:**
```elisp
(sly-describe-symbol symbol)    ; Show documentation
(sly-describe-function name)    ; Describe function
(sly-apropos pattern)           ; Search symbols
```

### 7.2 Public SLYNK API

**Server control:**
```lisp
(slynk:create-server &key port style dont-close)
(slynk:stop-server port)
```

**Evaluation:**
```lisp
(slynk:interactive-eval string)
(slynk:eval-and-grab-output string)
(slynk:pprint-eval string)
```

**Introspection:**
```lisp
(slynk:find-definitions-for-emacs name)
(slynk:xref type name)
(slynk:apropos-list-for-emacs pattern)
```

---

## 8. Code Statistics

| Component | Files | Lines | Purpose |
|-----------|-------|-------|---------|
| Core Elisp | 1 | 7,511 | Main functionality |
| Lib Elisp | 8 | ~8,000 | Utilities |
| Contrib Elisp | 14 | ~6,600 | Optional features |
| Core SLYNK | 10 | ~10,000 | Server |
| Backends | 12 | ~16,000 | Implementation-specific |
| Contrib SLYNK | 9 | ~5,500 | Server extensions |
| Tests | 8 | ~1,500 | Test suite |
| **Total** | **62** | **~55,000** | |

---

*Document generated from SLY source code analysis. Last updated: 2025.*
