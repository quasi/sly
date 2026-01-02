# SLY Unit Testing Guide

## What This Is

SLY has a comprehensive unit test suite that tests Emacs Lisp functionality **without requiring a live Lisp connection**. This guide explains how the tests work, how to write new tests, and how to extend the test framework.

## Why Unit Tests Matter

SLY is a client-server system where Emacs (client) talks to Common Lisp (server). But much of SLY's code is pure Elisp—buffer manipulation, parsing, UI logic—that works independently. Unit tests verify this code works correctly without the overhead of starting a Lisp process.

**Benefits:**
- Fast feedback (tests run in seconds, not minutes)
- Isolated testing (no external dependencies)
- Safe refactoring (you'll know immediately if you break something)
- Clear specifications (tests document how code should behave)

## Your First Unit Test

Here's a complete working test you can run right now:

```elisp
;; File: test/sly-example-unit-tests.el
;;; -*- lexical-binding: t; -*-

(require 'ert)
(require 'sly-unit-tests "lib/sly-unit-tests")

(ert-deftest sly-example-unit--string-manipulation ()
  "Test that string-upcase works."
  (should (equal "HELLO" (string-upcase "hello"))))

(provide 'sly-example-unit-tests)
```

Run it:
```bash
make check-unit
```

Expected output:
```
Running 131 tests...
   passed  131/131  sly-...-unit--...
```

You now have a working test. See [Common Test Patterns](#common-test-patterns) to learn what you can test.

## How Tests Are Organized

All unit tests follow this structure:

```
lib/sly-unit-tests.el          # Test framework and utilities
test/sly-test-mocks.el         # Mock objects for testing without Lisp
test/sly-test-coverage.el      # Coverage configuration
test/sly-*-unit-tests.el       # 7 test modules covering SLY features
```

### Test Modules

Each module tests a specific SLY library:

| Module | Tests | Coverage |
|--------|-------|----------|
| `sly-buttons-unit-tests.el` | 17 | Button system (overlays, types, keymaps) |
| `sly-common-unit-tests.el` | 21 | Buffer manipulation and refresh utilities |
| `sly-completion-unit-tests.el` | 18 | Completion system and annotation |
| `sly-hyperspec-unit-tests.el` | 18 | HyperSpec symbol lookup and deduplication |
| `sly-messages-unit-tests.el` | 24 | Message formatting, prompts, and feedback |
| `sly-parse-unit-tests.el` | 33 | Parsing, syntax detection, form location |
| `sly-test-mocks.el` | — | Mock implementations for testing RPC calls |

**Total: 131 unit tests across 7 modules**

## Common Test Patterns

### Pattern 1: Testing Pure Functions

```elisp
(ert-deftest sly-example-unit--string-trim ()
  "Test `string-trim' removes whitespace."
  (should (equal "hello" (string-trim "  hello  "))))
```

Use `should` to assert the expected value is true.

### Pattern 2: Testing with Temp Buffers

Many SLY functions work with Emacs buffers. Use `sly-unit-with-temp-lisp-buffer` to create a test buffer:

```elisp
(ert-deftest sly-example-unit--find-symbol-at-point ()
  "Test that symbol-at-point works in Lisp code."
  (sly-unit-with-temp-lisp-buffer "foo bar baz"
    (goto-char 5)  ; Position at "bar"
    (should (equal "bar" (symbol-at-point)))))
```

The macro creates a temporary Lisp-mode buffer with your content, then executes the test code. The buffer is automatically cleaned up afterward.

### Pattern 3: Testing with Point Markers

Use the `|` character to mark where point should be:

```elisp
(ert-deftest sly-example-unit--complete-symbol ()
  "Test completion at cursor position."
  (sly-unit-with-temp-lisp-buffer-point "sym|bol"
    ;; Point is now between "sym" and "bol"
    (should (equal 4 (point)))))
```

The `|` is automatically removed and point is placed there.

### Pattern 4: Testing with Mocked Functions

When your code calls other functions, mock them to control behavior:

```elisp
(ert-deftest sly-example-unit--with-mocked-function ()
  "Test that our function calls helper correctly."
  (sly-unit-with-mock
      ((helper-function (lambda () "mocked result")))
    (should (equal "mocked result" (helper-function)))))
```

This temporarily replaces `helper-function` with the lambda for the duration of the test.

### Pattern 5: Testing with a Stubbed Connection

Some SLY functions check if a connection exists. Stub it:

```elisp
(ert-deftest sly-example-unit--with-stubbed-connection ()
  "Test code that requires a connection."
  (sly-unit-with-stubbed-connection
    ;; Inside this block, (sly-connected-p) returns t
    (should (sly-connected-p))))
```

This mocks `sly-connected-p`, `sly-current-connection`, `sly-current-package`, and `sly-buffer-package`.

### Pattern 6: Testing RPC Calls with Mock Responses

When your code evaluates Lisp, mock the responses:

```elisp
(ert-deftest sly-example-unit--eval-with-mock-response ()
  "Test code that calls sly-eval."
  (sly-test-with-mock-eval
      (((slynk:my-function "arg") . "result from Lisp"))
    ;; Inside this block, (sly-eval '(slynk:my-function "arg"))
    ;; returns "result from Lisp"
    (let ((result (sly-eval '(slynk:my-function "arg"))))
      (should (equal "result from Lisp" result)))))
```

Map each Lisp call to its response. Responses can be values or functions that compute a result.

### Pattern 7: Testing Error Conditions

Use `should-error` to verify errors are raised when expected:

```elisp
(ert-deftest sly-example-unit--error-on-bad-input ()
  "Test that function errors on invalid input."
  (should-error (my-function-that-validates nil)))
```

Or check the error type:

```elisp
(ert-deftest sly-example-unit--type-error ()
  "Test that function raises type-error."
  (should-error
    (my-function-that-validates 42)
    :type 'type-error))
```

### Pattern 8: Testing Overlays and Buttons

Create test overlays with the overlay test utilities:

```elisp
(ert-deftest sly-example-unit--overlay-properties ()
  "Test that overlay has correct properties."
  (sly-unit-with-overlays
      ((ov 1 10 'my-prop "value"))
    (should (equal "value" (overlay-get ov 'my-prop)))))
```

The macro creates overlays, executes your code, then deletes them.

## Testing Framework Reference

### Macros for Test Setup

These macros create test environments:

#### `sly-unit-with-temp-lisp-buffer`
Create a temporary Lisp-mode buffer.

```elisp
(sly-unit-with-temp-lisp-buffer "(defun foo () 42)"
  (goto-char 10)
  (test-code-here))
```

#### `sly-unit-with-temp-lisp-buffer-point`
Create a temp buffer where `|` marks the point position.

```elisp
(sly-unit-with-temp-lisp-buffer-point "(defun |foo () 42)"
  ;; Point is between "(" and "foo"
  (test-code-here))
```

#### `sly-unit-with-mock`
Temporarily replace functions.

```elisp
(sly-unit-with-mock
    ((my-function (lambda (x) (* x 2)))
     (another-function (lambda () "test")))
  ;; my-function and another-function are replaced
  (test-code-here))
```

#### `sly-unit-with-stubbed-connection`
Mock connection-related functions.

```elisp
(sly-unit-with-stubbed-connection
  ;; (sly-connected-p) => t
  ;; (sly-current-connection) => 'mock-connection
  ;; (sly-current-package) => "CL-USER"
  (test-code-here))
```

#### `sly-test-with-mock-eval`
Mock `sly-eval` and `sly-eval-async` with predefined responses.

```elisp
(sly-test-with-mock-eval
    (((slynk:my-function "a") . "result")
     ((slynk:other-function) . (1 2 3)))
  ;; sly-eval now returns mocked values
  (test-code-here))
```

### Assertion Helpers

#### `should`
Assert that a value is true.

```elisp
(should (= 5 (+ 2 3)))
(should (member 'foo '(foo bar)))
```

#### `should-not`
Assert that a value is false.

```elisp
(should-not (= 5 (+ 2 2)))
```

#### `should-error`
Assert that code raises an error.

```elisp
(should-error (error "oops"))
(should-error (/ 1 0) :type 'arith-error)
```

#### `sly-unit-should-match`
Assert that a string matches a regex.

```elisp
(sly-unit-should-match "^foo" "foobar")
```

#### `sly-unit-should-equal-normalized`
Assert that strings are equal after normalizing whitespace.

```elisp
(sly-unit-should-equal-normalized
  "hello world"
  "hello   world")  ; Extra spaces are ignored
```

### Mock Object Utilities

#### `sly-test-with-mock-connection`
Set up a mock connection without eval responses.

```elisp
(sly-test-with-mock-connection
  ;; Connection exists but no eval behavior
  (should (sly-connected-p)))
```

#### `sly-test-mock-assert-called`
Assert that a mocked function was called.

```elisp
(sly-test-with-mock-eval (((slynk:foo) . t))
  (sly-eval '(slynk:foo))
  (sly-test-mock-assert-called 'sly-eval))
```

#### `sly-test-mock-assert-called-with`
Assert that a mocked function was called with specific arguments.

```elisp
(sly-test-with-mock-eval (((slynk:foo "arg") . t))
  (sly-eval '(slynk:foo "arg"))
  (sly-test-mock-assert-called-with 'sly-eval
    '((slynk:foo "arg"))))
```

## Adding Tests to Your Module

### Step 1: Choose Your Test Module

Decide which test file your test belongs in:

- **Buffer/UI code?** → `sly-common-unit-tests.el`
- **Button system?** → `sly-buttons-unit-tests.el`
- **Completion logic?** → `sly-completion-unit-tests.el`
- **Parsing/syntax?** → `sly-parse-unit-tests.el`
- **Message formatting?** → `sly-messages-unit-tests.el`
- **HyperSpec lookup?** → `sly-hyperspec-unit-tests.el`
- **New feature?** → Create `sly-myfeature-unit-tests.el`

### Step 2: Organize Your Tests

Group related tests under a section header:

```elisp
;;;; Tests for my-new-function
;;;;

(ert-deftest sly-example-unit--my-new-function-basic ()
  "Test basic behavior."
  (should ...))

(ert-deftest sly-example-unit--my-new-function-edge-case ()
  "Test edge case."
  (should ...))
```

### Step 3: Name Your Test

Follow the naming convention:

```
sly-[module]-unit--[function-name]-[scenario]
```

Examples:
- `sly-buttons-unit--level-default` (module: buttons, function: level, scenario: default value)
- `sly-completion-unit--string-detection-in-comment` (module: completion, function: string-detection, scenario: in-comment)
- `sly-parse-unit--form-at-point-nested-parens` (module: parse, function: form-at-point, scenario: nested-parens)

### Step 4: Write a Clear Docstring

The docstring is the first thing maintainers read. Make it clear:

```elisp
;; Good - describes what's being tested and the scenario
(ert-deftest sly-buttons-unit--level-returns-zero-by-default ()
  "Test that `sly-button--level' returns 0 when not set."
  ...)

;; Bad - too vague
(ert-deftest sly-buttons-unit--test-level ()
  "Test level."
  ...)
```

### Step 5: Write Your Test Body

Keep it focused and readable:

```elisp
(ert-deftest sly-example-unit--my-function-basic ()
  "Test `my-function' returns expected value."
  ;; Setup: create test data
  (let ((input '(1 2 3)))
    ;; Action: call the function
    (let ((result (my-function input)))
      ;; Assertion: verify the result
      (should (equal '(2 3 4) result)))))
```

## Module Details

### sly-buttons-unit-tests.el

Tests button system functionality:

- **Button levels**: How deeply nested a button is (for visual styling)
- **Button types**: Inheritance hierarchy (sly-action, sly-part)
- **Button creation**: Making clickable buttons with properties
- **Button finding**: Querying buttons at positions and by type
- **Keymaps**: Button navigation and interaction bindings

**Key utilities tested:**
- `sly-button--level`: Get/set button nesting level
- `sly-button--overlays-in`: Find buttons in a region
- `sly-make-action-button`: Create clickable buttons
- `sly-button-at`: Find button at point

**When to add tests:** When modifying button behavior, button type hierarchies, or button keymaps.

### sly-common-unit-tests.el

Tests common buffer manipulation utilities:

- **Buffer refresh**: Managing point and regions during buffer modifications
- **Overlay management**: Creating and manipulating overlays
- **Narrowing**: Working with narrowed regions
- **Point recovery**: Keeping point in correct position during edits

**Key utilities tested:**
- `sly--call-refreshing`: Execute code while preserving buffer state
- `sly-refreshing`: Macro version for cleaner code
- Buffer content manipulation with overlays

**When to add tests:** When modifying how SLY updates buffers (REPLs, inspector, etc.).

### sly-completion-unit-tests.el

Tests completion system:

- **Annotation display**: Showing completion metadata
- **String/comment detection**: Suppressing completion in wrong contexts
- **Caching**: Completion result caching
- **Wrapper behavior**: How completion wraps Lisp responses

**Key utilities tested:**
- `sly-completion-at-point`: Main completion function
- Annotation formatting
- Context detection (string vs code)

**When to add tests:** When modifying completion behavior or adding new metadata.

### sly-hyperspec-unit-tests.el

Tests HyperSpec symbol lookup:

- **Package prefix stripping**: Removing CL::, COMMON-LISP:: prefixes
- **Symbol deduplication**: Removing duplicate symbols
- **Case sensitivity**: Handling symbol case correctly
- **Special characters**: Symbols with unusual names

**Key utilities tested:**
- Symbol insertion and lookup
- Package prefix handling
- Hash table management

**When to add tests:** When modifying HyperSpec lookup or symbol handling.

### sly-messages-unit-tests.el

Tests message and logging utilities:

- **Message formatting**: Prefix, truncation, styling
- **Message levels**: Info, warning, error distinctions
- **Prompt formatting**: User-facing messages
- **Visual feedback**: Flashing regions, highlighting

**Key utilities tested:**
- `sly-message`: Information messages
- `sly-warning`: Warning messages
- `sly-error`: Error messages
- `sly-user-error`: User-caused errors
- `sly-flash-region`: Visual feedback

**When to add tests:** When modifying message formatting or adding user-facing feedback.

### sly-parse-unit-tests.el

Tests parsing and syntax utilities:

- **Syntax detection**: Character classification (parens, whitespace, word)
- **String/comment detection**: Is point in a string or comment?
- **Form location**: Finding expressions at point
- **List navigation**: Moving through nested lists
- **Pattern matching**: Recognizing code patterns

**Key utilities tested:**
- `sly-parse-form-at-point`: Find expression at point
- Character syntax tables
- String and comment boundary detection
- Nested list traversal

**When to add tests:** When modifying parsing logic or syntax detection.

## Running Tests

### Run All Unit Tests
```bash
make check-unit
```

This runs all 131 unit tests across all modules (buttons, common, completion, hyperspec, messages, parse). Unit tests do not require a Lisp connection.

### Run Unit Tests with Coverage
```bash
make check-unit-coverage
```

This runs all unit tests with coverage tracking and generates a `coverage/` directory with an HTML report. Requires the `undercover.el` package.

### Run Specific Test Interactively
To run a specific test or subset of tests, load the test file in Emacs and use ERT:

```elisp
;; Load the tests
M-x load-file RET test/sly-buttons-unit-tests.el RET

;; Run all tests in that file
M-x ert RET t RET

;; Run a specific test
M-x ert RET sly-buttons-unit--level-default RET

;; Run all unit tests matching a pattern
M-x ert RET ^sly-buttons-unit-- RET
```

### Run Integration Tests (Requires Lisp)

These tests require a running Lisp implementation and test the full client-server integration:

```bash
# Run all integration tests (core + contribs)
make check

# Run core integration tests only
make check-core

# Run specific contrib tests
make check-mrepl      # mREPL contrib tests
make check-stickers   # Stickers contrib tests
make check-autodoc    # Autodoc contrib tests

# Run with specific Lisp implementation
make check LISP=ccl       # Use CCL
make check LISP=sbcl      # Use SBCL
make check LISP=/path/to/lisp  # Use custom Lisp
```

**Note**: The `check-%` pattern only works for contribs, not unit test modules. There are no `make check-buttons` or `make check-parse` targets. Use `make check-unit` to run all unit tests.

## Troubleshooting

### Test Fails with "No mock response registered"

**Cause**: You're calling `sly-eval` inside a test without providing a mock response.

**Fix**: Use `sly-test-with-mock-eval` and register the Lisp call:

```elisp
;; Before (fails)
(ert-deftest sly-example-unit--eval-without-mock ()
  "This will fail."
  (sly-eval '(slynk:foo)))  ;; ERROR: No mock response

;; After (works)
(ert-deftest sly-example-unit--eval-with-mock ()
  "This works."
  (sly-test-with-mock-eval
      (((slynk:foo) . "result"))
    (sly-eval '(slynk:foo))))  ;; Returns "result"
```

### Test Fails with "sly-connected-p: not defined"

**Cause**: Your test code checks for a connection, but the connection functions aren't stubbed.

**Fix**: Wrap your test with `sly-unit-with-stubbed-connection`:

```elisp
;; Before (fails)
(ert-deftest sly-example-unit--needs-connection ()
  "This will fail."
  (when (sly-connected-p)  ;; ERROR: function not defined
    ...))

;; After (works)
(ert-deftest sly-example-unit--needs-connection ()
  "This works."
  (sly-unit-with-stubbed-connection
    (when (sly-connected-p)
      ...)))
```

### Test Passes Locally But Fails in CI

**Cause**: Test depends on external state (Lisp connection, file system, environment).

**Fix**: Make test truly isolated by mocking all external dependencies:

```elisp
;; Before (fragile - depends on real connection)
(ert-deftest sly-example-unit--depends-on-connection ()
  "Test (broken in CI)."
  (when (sly-connected-p)
    (let ((result (sly-eval '(slynk:my-function))))
      ...)))

;; After (isolated)
(ert-deftest sly-example-unit--isolated-test ()
  "Test (works everywhere)."
  (sly-test-with-mock-eval
      (((slynk:my-function) . "mocked result"))
    (let ((result (sly-eval '(slynk:my-function))))
      ...)))
```

### "Point is at..." Assertions Fail

**Cause**: Using `|` marker but checking wrong position.

**Fix**: Remember that `|` is removed, so positions shift. Be careful with adjacent markers:

```elisp
;; Before (wrong)
(sly-unit-with-temp-lisp-buffer-point "|foo"
  (should (= 1 (point))))  ;; Fails! Point is at 1, char "f" is at 1

;; After (right - understand positions)
(sly-unit-with-temp-lisp-buffer-point "|foo"
  (should (= (point-min) (point)))  ;; Correct
  (should (eq ?f (char-after)))     ;; Point is before 'f'
  )
```

## Advanced: Extending the Test Framework

### Adding New Test Utilities

If you find yourself repeating setup code, create a reusable macro in `lib/sly-unit-tests.el`:

```elisp
;; In lib/sly-unit-tests.el
(defmacro sly-unit-with-my-test-setup (&rest body)
  "Setup common test environment for my tests."
  (declare (indent 0) (debug t))
  `(sly-unit-with-stubbed-connection
     (sly-test-with-mock-eval (...)
       ,@body)))
```

Then use it in your tests:

```elisp
(ert-deftest sly-example-unit--using-new-utility ()
  "Test using new utility."
  (sly-unit-with-my-test-setup
    (test-code-here)))
```

### Adding New Mock Responses

For complex Lisp responses, create helper functions in `test/sly-test-mocks.el`:

```elisp
;; In test/sly-test-mocks.el
(defun sly-test-mock-completion-response (completions)
  "Create a mock completion response."
  (list completions (car completions)))

;; In your test
(ert-deftest sly-example-unit--complex-mock ()
  "Test with complex mock response."
  (sly-test-with-mock-eval
      (((slynk:simple-completions "foo" "CL-USER")
        . (sly-test-mock-completion-response '("foobar" "foobaz"))))
    ...))
```

## Next Steps

- **Write a test**: Pick one of the 7 modules and add a test following the patterns above
- **Run the tests**: `make check` to see your test pass
- **Explore examples**: Look at existing tests in each module to see patterns in action
- **Contribute**: Submit your tests as part of a bug fix or feature PR

## Related Documentation

- [CONTRIBUTING.md](../CONTRIBUTING.md) — How to contribute to SLY
- [SLY-ARCHITECTURE.md](../doc/SLY-ARCHITECTURE.md) — Overall system design
- [SLYNK-PROTOCOL.md](../doc/SLYNK-PROTOCOL.md) — Client-server protocol details

## Questions?

If something is unclear, check the existing tests in the module you're working with. Each test is a worked example of how to test similar code.
