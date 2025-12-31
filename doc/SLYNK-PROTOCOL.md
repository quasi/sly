# SLYNK Protocol Specification

This document provides an exhaustive specification of the SLYNK protocol used by SLY to communicate between Emacs and Common Lisp implementations.

## Overview

SLYNK (the server component of SLY) uses a text-based protocol built on S-expressions transmitted over TCP. The protocol is bidirectional, supporting both synchronous RPC (Remote Procedure Call) and asynchronous events.

**Historical Note:** SLY is a fork of SLIME, and SLYNK is a fork of SWANK. The protocol remains largely compatible but has diverged in several areas, particularly around channels and the MREPL system.

---

## 1. Wire Format

### 1.1 Message Structure

Every message follows this format:

```
┌─────────────────┬──────────────────────────────────────┐
│  6-byte header  │  UTF-8 encoded S-expression payload  │
└─────────────────┴──────────────────────────────────────┘
```

### 1.2 Header Format

The header is exactly 6 ASCII hexadecimal characters representing the byte length of the payload (not including the header itself).

```
"00001a" = 26 bytes of payload
"000100" = 256 bytes of payload
"ffffff" = 16,777,215 bytes (maximum message size)
```

**Encoding (Lisp side):**
```lisp
;; slynk-rpc.lisp:150-156
(defun write-header (stream length)
  (declare (type (unsigned-byte 24) length))
  (loop for c across (format nil "~6,'0x" length)
        do (write-byte (char-code c) stream)))
```

**Decoding (Emacs side):**
```elisp
;; sly.el:1719
(defsubst sly-net-decode-length ()
  (string-to-number (buffer-substring (point) (+ (point) 6)) 16))
```

### 1.3 Payload Format

The payload is a UTF-8 encoded S-expression that must be readable by both Emacs Lisp and Common Lisp. The S-expression is a list where:

- The first element (car) is a keyword identifying the message type
- Remaining elements are message-specific arguments

**Example messages:**
```lisp
(:emacs-rex (slynk:connection-info) "COMMON-LISP-USER" t 1)
(:return (:ok (:pid 12345 :lisp-implementation (:type "SBCL"))) 1)
(:write-string "Hello, world!" :repl-result)
```

### 1.4 Package Context

Messages are read in the `SLYNK-IO-PACKAGE` (Lisp) or `sly-io-package` (Emacs) which provides a clean namespace for the protocol symbols.

**Lisp side:**
```lisp
;; slynk-rpc.lisp:11-17
(defpackage slynk-rpc
  (:use :cl)
  (:export ...))

(defparameter *slynk-io-package*
  (let ((p (make-package :slynk-io-package :use '())))
    (import '(nil t quote) p)
    p))
```

---

## 2. Message Types

### 2.1 Emacs → Lisp Messages

These messages originate in Emacs and are sent to the SLYNK server.

#### 2.1.1 `:emacs-rex` - Remote Execution Request

The primary RPC mechanism. Asks Lisp to evaluate a form and return the result.

```lisp
(:emacs-rex FORM PACKAGE THREAD-ID REQUEST-ID &rest EXTRA-OPTIONS)
```

| Field | Type | Description |
|-------|------|-------------|
| `FORM` | sexp | Lisp form to evaluate (typically `(slynk:function-name args...)`) |
| `PACKAGE` | string | Package name for evaluation context |
| `THREAD-ID` | `t` \| `:find-existing` \| integer | Target thread for evaluation |
| `REQUEST-ID` | integer | Unique ID for correlating response |
| `EXTRA-OPTIONS` | plist | Additional options (rarely used) |

**Thread-ID values:**
- `t` - Spawn a new worker thread
- `:find-existing` - Use an existing active thread
- `integer` - Target a specific thread by ID

**Example:**
```lisp
(:emacs-rex (slynk:autodoc '("format" slynk::%cursor-marker%))
            "COMMON-LISP-USER" t 42)
```

#### 2.1.2 `:emacs-interrupt` - Interrupt Thread

Sends an interrupt signal to a running evaluation.

```lisp
(:emacs-interrupt THREAD-ID)
```

**Implementation:**
```lisp
;; slynk.lisp:1290
((:emacs-interrupt thread-id)
 (interrupt-thread connection thread-id))
```

#### 2.1.3 `:emacs-return` - Return Value to Waiting Thread

Returns a value to a Lisp thread that requested input from Emacs.

```lisp
(:emacs-return THREAD-ID TAG VALUE)
```

Used when Lisp calls `slynk:eval-in-emacs` and needs a return value.

#### 2.1.4 `:emacs-return-string` - Return String Input

Returns user-entered string to a waiting `read-string` or `read-from-minibuffer` request.

```lisp
(:emacs-return-string THREAD-ID TAG STRING)
```

#### 2.1.5 `:emacs-pong` - Ping Response

Responds to a ping from the server.

```lisp
(:emacs-pong THREAD-ID TAG)
```

#### 2.1.6 `:emacs-channel-send` - Channel Message

Sends a message to a specific channel (see Section 5).

```lisp
(:emacs-channel-send CHANNEL-ID MESSAGE)
```

#### 2.1.7 `:emacs-skipped-packet` - Skipped Packet Notification

Informs the server that a packet was intentionally skipped.

```lisp
(:emacs-skipped-packet PACKET)
```

### 2.2 Lisp → Emacs Messages

These messages originate in the SLYNK server and are sent to Emacs.

#### 2.2.1 `:return` - RPC Response

Returns the result of an `:emacs-rex` request.

```lisp
(:return VALUE REQUEST-ID)
```

**VALUE formats:**
- `(:ok RESULT)` - Successful evaluation, RESULT is the return value
- `(:abort CONDITION)` - Evaluation aborted, CONDITION describes why
- `(:error CONDITION)` - Error during evaluation (distinct from `:abort`)

**Example:**
```lisp
(:return (:ok ((:label "Name:" :value "FOO"))) 42)
(:return (:abort "User interrupt") 43)
```

#### 2.2.2 `:debug` - Enter Debugger

Signals that a condition occurred and the debugger should be activated.

```lisp
(:debug THREAD-ID LEVEL CONDITION RESTARTS FRAMES CONTINUATIONS)
```

| Field | Type | Description |
|-------|------|-------------|
| `THREAD-ID` | integer | Thread that triggered the condition |
| `LEVEL` | integer | Debugger nesting level (1-based) |
| `CONDITION` | list | `(DESCRIPTION TYPE EXTRAS...)` |
| `RESTARTS` | list | Available restart options |
| `FRAMES` | list | Initial backtrace frames |
| `CONTINUATIONS` | list | Pending continuations |

**CONDITION structure:**
```lisp
("Division by zero" "DIVISION-BY-ZERO" (:references ...))
```

**RESTARTS structure:**
```lisp
(("ABORT" "Return to REPL")
 ("CONTINUE" "Return zero")
 ("USE-VALUE" "Supply a value to use"))
```

**FRAMES structure:**
```lisp
((0 "(/ 1 0)" (:restartable t))
 (1 "(EVAL (/ 1 0))" (:restartable nil))
 ...)
```

#### 2.2.3 `:debug-activate` - Activate Debugger Display

Tells Emacs to display the debugger buffer.

```lisp
(:debug-activate THREAD-ID LEVEL)
```

Sent after `:debug` when the debugger is ready for interaction.

#### 2.2.4 `:debug-return` - Exit Debugger

Signals that the debugger is exiting.

```lisp
(:debug-return THREAD-ID LEVEL STEPPING-P)
```

| Field | Description |
|-------|-------------|
| `STEPPING-P` | If non-nil, stepping through code |

#### 2.2.5 `:debug-condition` - Debugger Condition

Notifies about a condition in the debugger itself.

```lisp
(:debug-condition THREAD-ID MESSAGE)
```

#### 2.2.6 `:write-string` - Output Text

Sends text output to be displayed.

```lisp
(:write-string STRING &optional TARGET)
```

**TARGET values:**
- `:repl-result` - REPL result output
- `:repl-stderr` - Standard error
- `nil` or omitted - Standard output

#### 2.2.7 `:read-string` - Request String Input

Requests that Emacs read a string from the user.

```lisp
(:read-string THREAD-ID TAG)
```

Emacs responds with `:emacs-return-string`.

#### 2.2.8 `:read-from-minibuffer` - Request Minibuffer Input

Requests input via Emacs minibuffer.

```lisp
(:read-from-minibuffer THREAD-ID TAG PROMPT INITIAL-VALUE)
```

#### 2.2.9 `:y-or-n-p` - Yes/No Question

Asks a yes-or-no question.

```lisp
(:y-or-n-p THREAD-ID TAG QUESTION)
```

#### 2.2.10 `:eval` - Evaluate in Emacs

Requests Emacs to evaluate an Emacs Lisp form.

```lisp
(:eval THREAD-ID TAG FORM-STRING)
```

**Example:**
```lisp
(:eval 1 42 "(message \"Hello from Lisp!\")")
```

#### 2.2.11 `:eval-no-wait` - Evaluate in Emacs (No Response)

Like `:eval` but doesn't wait for a response.

```lisp
(:eval-no-wait FORM-STRING)
```

#### 2.2.12 `:new-package` - Package Changed

Notifies that the current package changed.

```lisp
(:new-package PACKAGE-NAME PROMPT-STRING)
```

#### 2.2.13 `:new-features` - Features List Update

Sends updated `*features*` list.

```lisp
(:new-features FEATURES-LIST)
```

#### 2.2.14 `:indentation-update` - Indentation Cache Update

Sends indentation information for symbols.

```lisp
(:indentation-update INDENTATION-INFO)
```

**INDENTATION-INFO format:**
```lisp
((symbol1 . (indent-spec1))
 (symbol2 . (indent-spec2))
 ...)
```

#### 2.2.15 `:ed` - Edit Location

Requests Emacs to open a file/location for editing.

```lisp
(:ed WHAT)
```

**WHAT formats:**
- String - File path
- `(:filename PATH :position POS)` - File with position
- `(:function NAME)` - Find function definition

#### 2.2.16 `:inspect` - Open Inspector

Opens the inspector on an object.

```lisp
(:inspect WHAT THREAD-ID TAG)
```

#### 2.2.17 `:ping` - Ping Request

Server pings Emacs to check liveness.

```lisp
(:ping THREAD-ID TAG)
```

Emacs responds with `:emacs-pong`.

#### 2.2.18 `:channel-send` - Channel Message

Sends a message from a channel.

```lisp
(:channel-send CHANNEL-ID MESSAGE)
```

#### 2.2.19 `:background-message` - Background Notification

Displays a message without blocking.

```lisp
(:background-message MESSAGE)
```

#### 2.2.20 `:reader-error` - Protocol Parse Error

Reports a parsing error in the protocol stream.

```lisp
(:reader-error PACKET-STRING CONDITION)
```

#### 2.2.21 `:invalid-rpc` - Invalid RPC Request

Reports an invalid RPC request.

```lisp
(:invalid-rpc REQUEST-ID MESSAGE)
```

#### 2.2.22 `:invalid-channel` - Invalid Channel

Reports an error with a channel.

```lisp
(:invalid-channel CHANNEL-ID REASON)
```

---

## 3. RPC Mechanism

### 3.1 Request-Response Correlation

Each RPC request has a unique integer ID. Responses include this ID for correlation.

**Emacs side (sly.el:2328-2340):**
```elisp
(defun sly-dispatch-event (event &optional process)
  (sly-dcase event
    ((:emacs-rex form package thread continuation &rest extra-options)
     (when (and (sly-use-sigint-for-interrupt) (sly-busy-p))
       (sly-display-oneliner "; pipelined request... %S" form))
     (let ((id (cl-incf (sly-continuation-counter))))
       (sly-send `(:emacs-rex ,form ,package ,thread ,id ,@extra-options))
       (push (cons id continuation) (sly-rex-continuations))))

    ((:return value id)
     (let ((rec (assq id (sly-rex-continuations))))
       (cond (rec (setf (sly-rex-continuations)
                        (remove rec (sly-rex-continuations)))
                  (funcall (cdr rec) value))
             (t (error "Unexpected reply: %S %S" id value)))))))
```

### 3.2 Continuation Storage

Pending requests are stored in `sly-rex-continuations`, an alist of `(ID . CALLBACK)`.

### 3.3 Synchronous vs Asynchronous Evaluation

**Asynchronous (sly-eval-async):**
```elisp
(sly-eval-async '(slynk:connection-info)
  (lambda (result)
    (message "Got: %S" result)))
```

**Synchronous (sly-eval):**
```elisp
(let ((result (sly-eval '(slynk:connection-info))))
  (message "Got: %S" result))
```

Synchronous evaluation uses `accept-process-output` in a loop until the response arrives.

### 3.4 Result Values

RPC results are wrapped in status indicators:

| Status | Meaning | Example |
|--------|---------|---------|
| `:ok` | Success | `(:ok 42)` |
| `:abort` | Aborted (e.g., user interrupt) | `(:abort "User interrupt")` |
| `:error` | Evaluation error | `(:error "Undefined function")` |

---

## 4. Connection Establishment

### 4.1 Server Creation

**Lisp side:**
```lisp
(slynk:create-server :port 4005
                     :style :spawn
                     :dont-close nil)
```

**Communication styles:**
| Style | Description |
|-------|-------------|
| `:spawn` | Multithreaded (dedicated reader, control, worker threads) |
| `:sigio` | Single-threaded with SIGIO signals |
| `:fd-handler` | Single-threaded with file descriptor handlers |
| `nil` | Polling-based single-threaded |

### 4.2 Connection Sequence

```
┌─────────┐                              ┌──────────┐
│  Emacs  │                              │  SLYNK   │
└────┬────┘                              └────┬─────┘
     │                                        │
     │──── TCP Connect (port 4005) ──────────>│
     │                                        │
     │<─── [Optional: Secret Challenge] ──────│
     │                                        │
     │──── [Optional: Secret Response] ──────>│
     │                                        │
     │<─── Protocol handshake ────────────────│
     │     (version info in S-expression)     │
     │                                        │
     │──── (:emacs-rex (slynk:connection-info)│
     │                  "CL-USER" t 1) ───────>│
     │                                        │
     │<─── (:return (:ok (...)) 1) ───────────│
     │                                        │
     │     [Connection established]           │
     │                                        │
```

### 4.3 Authentication

Optional secret-based authentication via `~/.sly-secret` file.

**Lisp side (slynk.lisp:1020-1027):**
```lisp
(defun authenticate-client (stream)
  (let ((secret (sly-secret)))
    (when secret
      (set-stream-timeout stream 20)
      (let ((first-val (read-packet stream)))
        (unless (and (stringp first-val) (string= first-val secret))
          (error "Incoming connection doesn't know the password."))))))
```

### 4.4 Connection Structure

**Lisp side (slynk.lisp:206-243):**
```lisp
(defstruct (connection ...)
  (socket           ...)  ; Raw socket
  (socket-io        ...)  ; Stream for I/O
  (channel-counter  0)    ; Counter for channel IDs
  (channels         '())  ; Active channels
  (listeners        '())  ; Event listeners
  (inspectors       '())  ; Active inspectors
  (indentation-cache ...)  ; Symbol indentation info
  (communication-style nil))
```

---

## 5. Channels

Channels provide a mechanism for structured, stateful communication patterns that don't fit the simple RPC model.

### 5.1 Channel Concept

A channel is a named bidirectional communication endpoint. Each channel has:
- Unique numeric ID
- Associated thread
- Set of message handlers

### 5.2 Lisp Side (slynk.lisp:488-537)

```lisp
(defclass channel ()
  ((id     :initform (incf (channel-counter))
           :reader channel-id)
   (thread :initarg :thread :initform (current-thread)
           :reader channel-thread)
   (name   :initarg :name :initform nil)))

(defgeneric channel-send (channel selector args)
  (:documentation "Send MSG to CHANNEL."))
```

### 5.3 Emacs Side (sly.el:2641-2700)

```elisp
(cl-defstruct (sly-channel (:conc-name sly-channel.)
                           (:constructor sly-make-channel% ...))
  operations name id plist)

(defun sly-channel-send (channel message)
  (sly-send `(:emacs-channel-send ,(sly-channel.id channel) ,message)))
```

### 5.4 Use Cases

Channels are used by:
- **MREPL** - Multiple REPLs over single connection
- **Stickers** - Live value annotation streaming
- **Trace Dialog** - Trace event streaming

---

## 6. Threading Model

### 6.1 Multithreaded Mode (`:spawn`)

```
┌─────────────────────────────────────────────────────────┐
│                    SLYNK Server                         │
│                                                         │
│  ┌──────────────┐                                       │
│  │ Reader Thread│──reads from socket──┐                 │
│  └──────────────┘                     │                 │
│                                       ▼                 │
│                              ┌─────────────────┐        │
│                              │ Control Thread  │        │
│                              │  (dispatcher)   │        │
│                              └────────┬────────┘        │
│                                       │                 │
│               ┌───────────────────────┼───────────────┐ │
│               ▼                       ▼               ▼ │
│        ┌────────────┐          ┌────────────┐  ┌───────┐│
│        │ Worker #1  │          │ Worker #2  │  │  ...  ││
│        │ (eval req) │          │ (eval req) │  │       ││
│        └────────────┘          └────────────┘  └───────┘│
│                                                         │
└─────────────────────────────────────────────────────────┘
```

**Thread responsibilities:**

| Thread | Role |
|--------|------|
| Reader | Reads raw bytes from socket, decodes messages, sends to control |
| Control | Dispatches messages to appropriate workers/handlers |
| Worker | Executes RPC requests, sends results back |

### 6.2 Single-threaded Modes

For Lisps without threading or when threading is disabled:
- Messages queued in `sconn.event-queue`
- Processed sequentially during `process-requests`
- Interrupts handled via `events-enqueued` counter

### 6.3 Thread Targeting

```lisp
;; slynk.lisp:1220-1231
(defgeneric thread-for-evaluation (connection id)
  (:method ((conn multithreaded-connection) (id (eql t)))
    (spawn-worker-thread conn))   ; New thread
  (:method ((conn multithreaded-connection) (id (eql :find-existing)))
    (car (mconn.active-threads conn)))  ; Reuse existing
  (:method (conn (id integer))
    (find-thread id)))            ; Specific thread
```

---

## 7. Error Handling

### 7.1 Protocol-Level Errors

**Reader errors (slynk-rpc.lisp:27-31):**
```lisp
(define-condition slynk-reader-error (reader-error)
  ((packet :type string :initarg :packet
           :reader slynk-reader-error.packet)
   (cause :type reader-error :initarg :cause
          :reader slynk-reader-error.cause)))
```

### 7.2 Connection Errors

**Error handlers (slynk.lisp:318-341):**
```lisp
(defmacro with-slynk-error-handler ((connection) &body body)
  "Close the connection on internal `slynk-error's.")

(defmacro with-panic-handler ((connection) &body body)
  "Close the connection on unhandled `serious-condition's.")
```

### 7.3 Evaluation Errors

Evaluation errors enter the debugger rather than returning `:error`:
1. Condition occurs during evaluation
2. SLYNK sends `:debug` message
3. Emacs displays SLY-DB buffer
4. User selects restart or aborts
5. SLYNK sends `:return` with `:ok` (restart) or `:abort`

---

## 8. Common RPC Functions

These are the most commonly used SLYNK functions called via `:emacs-rex`:

### 8.1 Connection & Info

| Function | Purpose |
|----------|---------|
| `slynk:connection-info` | Get server info (Lisp impl, version, features) |
| `slynk:ping` | Check connection liveness |
| `slynk:quit-lisp` | Terminate Lisp process |

### 8.2 Evaluation

| Function | Purpose |
|----------|---------|
| `slynk:eval-and-grab-output` | Eval form, capture stdout |
| `slynk:interactive-eval` | Eval with pretty result |
| `slynk:interactive-eval-region` | Eval region string |
| `slynk:re-evaluate-defvar` | Re-eval defvar form |
| `slynk:pprint-eval` | Eval with pretty-printed result |

### 8.3 Compilation

| Function | Purpose |
|----------|---------|
| `slynk:compile-string-for-emacs` | Compile string, return notes |
| `slynk:compile-file-for-emacs` | Compile file, return notes |
| `slynk:load-file` | Load compiled/source file |
| `slynk:compile-notes-for-emacs` | Get compilation warnings |

### 8.4 Completion

| Function | Purpose |
|----------|---------|
| `slynk:simple-completions` | Basic prefix completion |
| `slynk:flex-completions` | Fuzzy/flex completion |
| `slynk:completions-for-character` | Character name completion |
| `slynk:completions-for-keyword` | Keyword completion |

### 8.5 Documentation

| Function | Purpose |
|----------|---------|
| `slynk:documentation-symbol` | Get symbol docstring |
| `slynk:describe-symbol` | Full symbol description |
| `slynk:describe-function` | Function signature and docs |
| `slynk:apropos-list-for-emacs` | Search symbols by name |

### 8.6 Definition Finding

| Function | Purpose |
|----------|---------|
| `slynk:find-definitions-for-emacs` | Find definition locations |
| `slynk:xref` | Cross-reference query |
| `slynk:xrefs` | Multiple xref queries |

### 8.7 Debugging

| Function | Purpose |
|----------|---------|
| `slynk:backtrace` | Get current backtrace |
| `slynk:debugger-info-for-emacs` | Full debugger state |
| `slynk:invoke-nth-restart-for-emacs` | Invoke restart by index |
| `slynk:sldb-abort` | Abort to top level |
| `slynk:sldb-continue` | Continue execution |
| `slynk:frame-source-location` | Source location for frame |
| `slynk:frame-locals-and-catch-tags` | Frame local variables |

### 8.8 Inspection

| Function | Purpose |
|----------|---------|
| `slynk:init-inspector` | Start inspector on value |
| `slynk:inspect-nth-part` | Inspect sub-object |
| `slynk:inspector-call-nth-action` | Invoke inspector action |
| `slynk:inspector-pop` | Go back in inspector |
| `slynk:inspector-reinspect` | Refresh inspector view |

### 8.9 Macroexpansion

| Function | Purpose |
|----------|---------|
| `slynk:slynk-macroexpand-1` | One-level macroexpand |
| `slynk:slynk-macroexpand-all` | Full macroexpand |
| `slynk:slynk-compiler-macroexpand-1` | Compiler macro expansion |

### 8.10 Package Management

| Function | Purpose |
|----------|---------|
| `slynk:list-all-package-names` | Get all package names |
| `slynk:set-package` | Set current package |

---

## 9. Version Compatibility

### 9.1 Protocol Version

The protocol version is exchanged during connection establishment. Mismatched versions produce warnings but usually work.

### 9.2 SLIME Compatibility

SLY maintains partial backward compatibility with SLIME:
- Core protocol messages are compatible
- Some SWANK functions have SLYNK equivalents
- The `slynk-retro` contrib provides translation layer

### 9.3 Implementation-Specific Behavior

Different Lisp implementations may:
- Support different communication styles
- Provide different levels of debugging info
- Have varying threading capabilities

---

## 10. Security Considerations

### 10.1 Network Exposure

By default, SLYNK listens on localhost only. Exposing it to the network is dangerous as it allows arbitrary code execution.

### 10.2 Secret File

The `~/.sly-secret` file provides basic authentication but should not be relied upon for security in hostile environments.

### 10.3 SSH Tunneling

For remote development, SSH tunneling is recommended:
```bash
ssh -L 4005:localhost:4005 remote-host
```

---

## Appendix A: Message Reference

### A.1 Quick Reference Table

| Direction | Message | Purpose |
|-----------|---------|---------|
| E→L | `:emacs-rex` | RPC request |
| E→L | `:emacs-interrupt` | Interrupt evaluation |
| E→L | `:emacs-return` | Return value to Lisp |
| E→L | `:emacs-return-string` | Return string input |
| E→L | `:emacs-pong` | Ping response |
| E→L | `:emacs-channel-send` | Channel message |
| L→E | `:return` | RPC response |
| L→E | `:debug` | Enter debugger |
| L→E | `:debug-activate` | Show debugger |
| L→E | `:debug-return` | Exit debugger |
| L→E | `:write-string` | Output text |
| L→E | `:read-string` | Request input |
| L→E | `:y-or-n-p` | Yes/no question |
| L→E | `:eval` | Eval in Emacs |
| L→E | `:channel-send` | Channel message |
| L→E | `:ping` | Ping request |
| L→E | `:new-package` | Package changed |
| L→E | `:indentation-update` | Indentation info |

### A.2 File References

| File | Lines | Content |
|------|-------|---------|
| `slynk/slynk-rpc.lisp` | 1-212 | Wire protocol implementation |
| `slynk/slynk.lisp` | 1273-1333 | Event dispatch |
| `sly.el` | 1504-1740 | Network layer |
| `sly.el` | 2248-2620 | Protocol dispatch |

---

*Document generated from SLY source code analysis. Last updated: 2025.*
