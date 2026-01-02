# Testing Guide for SLY Contrib Authors

This guide explains how to write tests for SLY contribs (extensions). It covers both unit tests and integration tests specific to contrib development.

## Overview

SLY contribs typically have both Elisp and Common Lisp components:
- **Elisp side** (`contrib/sly-<name>.el`): Emacs UI and client logic
- **Lisp side** (`slynk/contrib/slynk-<name>.lisp`): Server-side implementation

Both sides need testing to ensure the contrib works correctly.

## Test File Organization

Contrib tests follow this structure:

```
sly/
├── contrib/
│   ├── sly-mrepl.el              # Contrib Elisp code
│   └── test/
│       ├── sly-mrepl-tests.el    # Integration tests for sly-mrepl
│       └── sly-mrepl-unit-tests.el  # Unit tests (optional)
└── slynk/contrib/
    └── slynk-mrepl.lisp          # Contrib Lisp code
```

## Unit Tests vs Integration Tests

### Unit Tests (Optional but Recommended)

Unit tests verify pure Elisp functions without requiring a Lisp connection.

**When to write unit tests:**
- String parsing and formatting functions
- Data structure manipulation
- Pure utility functions
- UI state management (without network calls)

**Example unit test:**

```elisp
;;; sly-example-unit-tests.el --- Unit tests for sly-example -*- lexical-binding: t; -*-

(require 'ert)
(require 'sly-example "contrib/sly-example")
(require 'sly-unit-tests "lib/sly-unit-tests")
(require 'sly-test-mocks "test/sly-test-mocks")

(ert-deftest sly-example-unit--parse-output ()
  "Test output parsing function."
  (should (equal '(:result "success" :value 42)
                 (sly-example--parse-result "success: 42"))))

(ert-deftest sly-example-unit--with-mock-eval ()
  "Test with mocked slynk call."
  (sly-test-with-mock-eval
      (((slynk-example:get-data) . (:data "mocked")))
    (should (equal "mocked"
                   (plist-get (sly-example-fetch-data) :data)))))

(provide 'sly-example-unit-tests)
```

### Integration Tests (Required)

Integration tests verify the complete Elisp-Lisp interaction with a live connection.

**When to write integration tests:**
- RPC calls to slynk functions (slyfuns)
- REPL interactions
- Complete feature workflows
- Channel-based communication

**Example integration test:**

```elisp
;;; sly-example-tests.el --- Integration tests for sly-example -*- lexical-binding: t; -*-

(require 'sly-tests "lib/sly-tests")
(require 'sly-example "contrib/sly-example")

(def-sly-test example-basic-functionality ()
  "Test basic example functionality."
  (with-temp-buffer
    (lisp-mode)
    (sly-check "Can call example slyfun"
      (let ((result (sly-eval '(slynk-example:compute 2 3))))
        (equal 5 result)))))

(def-sly-test example-async-operation ()
  "Test async operation."
  (with-temp-buffer
    (lisp-mode)
    (let ((result nil))
      (sly-eval-async '(slynk-example:async-compute 10)
        (lambda (r) (setq result r)))
      (sly-sync-to-top-level 5)
      (sly-check "Async result received"
        (equal 20 result)))))

(provide 'sly-example-tests)
```

## Running Contrib Tests

### Running Your Contrib Tests

```bash
# Run integration tests for your contrib
make check-example  # Replace 'example' with your contrib name

# Run with specific Lisp
LISP=ccl make check-example
```

### During Development

```bash
# Quick unit test run (if you have unit tests)
emacs -Q --batch -L . -L contrib -L test \
  --eval "(require 'ert)" \
  --eval "(require 'sly-example-unit-tests)" \
  --eval "(ert-run-tests-batch-and-exit)"

# Integration test (requires running Lisp)
emacs -Q -L . -L contrib \
  --eval "(require 'sly)" \
  --eval "(setq sly-contribs '(sly-example))" \
  --eval "(setq inferior-lisp-program \"sbcl\")" \
  -f sly
# Then M-x sly-tests RET (tag contrib) RET
```

## Test Utilities for Contribs

### From `lib/sly-tests.el`

**`def-sly-test`**: Define integration tests

```elisp
(def-sly-test name (arguments)
  "Docstring."
  (test-body))
```

**`sly-check`**: Assert with descriptive error messages

```elisp
(sly-check "Operation succeeded"
  (equal expected actual))
```

**`sly-sync-to-top-level`**: Wait for Lisp to return to top level

```elisp
(sly-sync-to-top-level 5)  ; Wait up to 5 seconds
```

**`sly-wait-condition`**: Wait for a condition to become true

```elisp
(sly-wait-condition "Buffer updated" 3.0
  (equal (buffer-string) "Expected content"))
```

### From `lib/sly-unit-tests.el`

**`sly-unit-with-temp-lisp-buffer`**: Create temporary Lisp buffer

```elisp
(sly-unit-with-temp-lisp-buffer "(defun foo () nil)"
  (search-forward "foo")
  (should (sly-at-function-p)))
```

**`sly-unit-with-stubbed-connection`**: Stub connection functions

```elisp
(sly-unit-with-stubbed-connection
  (should (sly-connected-p)))
```

### From `test/sly-test-mocks.el`

**`sly-test-with-mock-eval`**: Mock RPC evaluation

```elisp
(sly-test-with-mock-eval
    (((slynk-example:foo "arg") . "result"))
  (should (equal "result" (sly-eval '(slynk-example:foo "arg")))))
```

## Common Testing Patterns

### Testing REPL Output

```elisp
(def-sly-test example-repl-output ()
  "Test REPL output formatting."
  (let ((output-buffer (sly-mrepl--find-buffer)))
    (with-current-buffer output-buffer
      (sly-eval '(slynk-example:print-message "Hello"))
      (sly-sync-to-top-level 2)
      (sly-check "Message appears in REPL"
        (save-excursion
          (goto-char (point-min))
          (search-forward "Hello" nil t))))))
```

### Testing Interactive Commands

```elisp
(def-sly-test example-interactive-command ()
  "Test interactive command."
  (with-temp-buffer
    (lisp-mode)
    (insert "(defun test-fn () 42)")
    (goto-char (point-min))
    (search-forward "test-fn")
    (call-interactively #'sly-example-describe-symbol)
    (sly-sync-to-top-level 2)
    (sly-check "Description buffer created"
      (get-buffer "*sly-example-description*"))))
```

### Testing Async Operations

```elisp
(def-sly-test example-async-with-callback ()
  "Test async operation with callback."
  (let ((callback-called nil)
        (callback-result nil))
    (sly-eval-async '(slynk-example:long-running-op)
      (lambda (result)
        (setq callback-called t
              callback-result result)))
    (sly-wait-condition "Callback invoked" 5.0
      callback-called)
    (sly-check "Callback received correct result"
      (equal "success" callback-result))))
```

### Testing Error Handling

```elisp
(def-sly-test example-error-handling ()
  "Test that errors are handled gracefully."
  (let ((error-signaled nil))
    (condition-case err
        (sly-eval '(slynk-example:will-error))
      (error
       (setq error-signaled t)))
    (sly-check "Error was signaled"
      error-signaled)))
```

### Testing Buffer Management

```elisp
(def-sly-test example-buffer-creation ()
  "Test special buffer creation."
  (let ((buf (sly-example-open-special-buffer)))
    (unwind-protect
        (progn
          (sly-check "Buffer created"
            (buffer-live-p buf))
          (sly-check "Buffer has correct mode"
            (with-current-buffer buf
              (eq major-mode 'sly-example-mode))))
      (when (buffer-live-p buf)
        (kill-buffer buf)))))
```

## Testing Checklist for Contribs

Before submitting your contrib, ensure:

### Unit Tests (if applicable)
- [ ] Pure functions have unit tests
- [ ] String parsing/formatting is tested
- [ ] Mock tests verify RPC interaction patterns
- [ ] All unit tests pass: `make check-unit` (if added to suite)

### Integration Tests (required)
- [ ] Main feature workflows are tested
- [ ] Interactive commands are tested
- [ ] REPL interactions are tested
- [ ] Error cases are tested
- [ ] Async operations are tested
- [ ] Tests pass on SBCL: `make check-<contrib>`
- [ ] Tests pass on CCL: `LISP=ccl make check-<contrib>`

### Test Quality
- [ ] Test names are descriptive
- [ ] Tests are independent (can run in any order)
- [ ] Tests clean up resources (buffers, overlays, timers)
- [ ] No hardcoded timeouts (use `sly-wait-condition`)
- [ ] Tests document expected behavior

### Documentation
- [ ] CONTRIBUTING.md updated with test instructions
- [ ] Contrib README mentions how to run tests
- [ ] Complex test scenarios are commented

## Adding Your Contrib to CI

To add your contrib tests to GitHub Actions CI:

1. **Add Makefile target** in main `Makefile`:

```makefile
check-example: CONTRIB_NAME=sly-example
check-example: SELECTOR=(tag contrib)
check-example: compile contrib/sly-example.elc contrib/test/sly-example-tests.elc
	$(EMACS) -Q --batch $(LOAD_PATH) -L test			\
		--eval "(require (quote sly))"				\
		--eval "(setq sly-contribs (quote (sly-example)))"	\
		--eval "(require (intern (format \"%s-tests\" (quote sly-example))))" \
		--eval "(setq inferior-lisp-program \"$(LISP)\")"	\
		--eval '(sly-batch-test (quote $(SELECTOR)))'
```

2. **Tests will run automatically** in the CI matrix with the `check-fancy` target if your contrib is part of `sly-fancy`.

## Debugging Test Failures

### Run Tests Interactively

```bash
emacs -Q -L . -L contrib \
  --eval "(require 'sly)" \
  --eval "(setq sly-contribs '(sly-example))" \
  --eval "(setq inferior-lisp-program \"sbcl\")" \
  -f sly
```

Then:
1. `M-x load-library RET contrib/test/sly-example-tests RET`
2. `M-x ert RET t RET` to run all tests interactively
3. Click on failed test to see details

### Enable SLY Event Tracing

```elisp
(setq sly-log-events t)
;; View events in *sly-events* buffer
```

### Add Debug Output

```elisp
(def-sly-test example-debug ()
  "Test with debug output."
  (let ((sly-inhibit-pipelining nil))
    (message "About to call slyfun...")
    (let ((result (sly-eval '(slynk-example:foo))))
      (message "Result: %S" result)
      (sly-check "Got result" result))))
```

## Common Pitfalls

### ❌ Hardcoded Timeouts

```elisp
;; BAD
(sleep-for 2)
(sly-check "Operation completed" (buffer-updated-p))
```

```elisp
;; GOOD
(sly-wait-condition "Buffer updated" 5.0
  (buffer-updated-p))
```

### ❌ Not Cleaning Up Resources

```elisp
;; BAD
(def-sly-test example-leak ()
  (let ((buf (generate-new-buffer "*test*")))
    (test-something buf)))
    ;; Buffer never killed!
```

```elisp
;; GOOD
(def-sly-test example-cleanup ()
  (let ((buf (generate-new-buffer "*test*")))
    (unwind-protect
        (test-something buf)
      (kill-buffer buf))))
```

### ❌ Tests Depend on Each Other

```elisp
;; BAD - Tests share state
(defvar example-test-state nil)

(def-sly-test example-setup ()
  (setq example-test-state "initialized"))

(def-sly-test example-use-state ()
  (sly-check "State exists" example-test-state))  ; Fails if run alone!
```

```elisp
;; GOOD - Each test is independent
(def-sly-test example-independent ()
  (let ((state "initialized"))
    (sly-check "State exists" state)))
```

### ❌ Assuming Specific Emacs Configuration

```elisp
;; BAD - Assumes user config
(def-sly-test example-assumes-config ()
  (sly-check "Feature enabled" some-user-option))
```

```elisp
;; GOOD - Explicit setup
(def-sly-test example-explicit ()
  (let ((some-user-option t))
    (sly-check "Feature works when enabled" (feature-p))))
```

## Additional Resources

- [CONTRIBUTING.md](../CONTRIBUTING.md#testing) - Main testing guide
- [lib/sly-tests.el](../lib/sly-tests.el) - Integration test framework source
- [lib/sly-unit-tests.el](../lib/sly-unit-tests.el) - Unit test utilities source
- [test/sly-test-mocks.el](../test/sly-test-mocks.el) - Mocking framework source
- Existing contrib tests: `contrib/test/sly-*-tests.el` for examples

## Getting Help

If you need help writing tests for your contrib:

1. Look at existing contrib tests for patterns
2. Check [GitHub Discussions](https://github.com/joaotavora/sly/discussions)
3. Ask in #lisp on Libera.Chat IRC
4. Open an issue labeled "question"

Happy testing! 🧪
