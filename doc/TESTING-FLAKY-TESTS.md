# Flaky Tests: Patterns and Solutions

This document describes common patterns that lead to flaky (non-deterministic) tests in SLY and how to fix them. A flaky test is one that sometimes passes and sometimes fails without any code changes.

## Table of Contents

- [Race Conditions](#race-conditions)
- [Timing Issues](#timing-issues)
- [State Leakage](#state-leakage)
- [Environment Dependencies](#environment-dependencies)
- [Resource Management](#resource-management)
- [Async Operations](#async-operations)
- [Prevention Checklist](#prevention-checklist)

## Race Conditions

### Problem: Tests Depend on Uncontrolled Timing

**Symptoms:**
- Test passes locally but fails in CI
- Test fails randomly, especially under load
- "Expected X but got Y" errors where Y is an intermediate state

**Example of Flaky Test:**

```elisp
(def-sly-test flaky-completion ()
  "Flaky test that races with async completion."
  (with-temp-buffer
    (insert "def")
    (sly-complete-symbol)  ; Async operation
    (sleep-for 0.1)  ; ❌ Fixed delay - may be too short or too long
    (sly-check "Completion appeared"
      (looking-at "defun"))))
```

**Solution: Use Condition-Based Waiting**

```elisp
(def-sly-test robust-completion ()
  "Robust test using condition-based waiting."
  (with-temp-buffer
    (insert "def")
    (sly-complete-symbol)
    ;; ✅ Wait for actual condition, not fixed time
    (sly-wait-condition "Completion appeared" 3.0
      (looking-at "defun"))
    (sly-check "Completion is correct"
      (equal "defun" (buffer-substring (point-min) (point))))))
```

**Key Points:**
- Never use `sleep-for` to wait for async operations
- Always use `sly-wait-condition` with a reasonable timeout
- Test for actual state changes, not elapsed time

## Timing Issues

### Problem: Hardcoded Timeouts

**Symptoms:**
- Tests fail on slower machines or in CI
- Tests pass when run individually but fail in batch
- Timeout values seem arbitrary (0.1s, 0.5s, etc.)

**Example of Flaky Test:**

```elisp
(def-sly-test flaky-repl-output ()
  "Flaky test with hardcoded timeout."
  (sly-eval '(cl:sleep 0.05))
  (sleep-for 0.1)  ; ❌ Assumes operation completes in 0.1s
  (sly-check "Output received"
    (buffer-contains-p "NIL")))
```

**Solution: Synchronize to Known State**

```elisp
(def-sly-test robust-repl-output ()
  "Robust test using synchronization."
  (sly-eval '(cl:sleep 0.05))
  ;; ✅ Wait for Lisp to return to top level
  (sly-sync-to-top-level 5)
  (sly-check "Output received"
    (buffer-contains-p "NIL")))
```

**Alternative: Event-Based Waiting**

```elisp
(def-sly-test robust-repl-output-v2 ()
  "Robust test using event tracking."
  (let ((output-received nil))
    (sly-eval-async '(cl:sleep 0.05)
      (lambda (result)
        (setq output-received t)))
    ;; ✅ Wait for actual callback invocation
    (sly-wait-condition "Output received" 5.0
      output-received)
    (sly-check "Result is correct"
      output-received)))
```

**Key Points:**
- Use `sly-sync-to-top-level` after synchronous evaluations
- Use `sly-wait-condition` with callbacks for async operations
- Timeout values should be generous (3-10 seconds for complex operations)

## State Leakage

### Problem: Tests Affect Each Other

**Symptoms:**
- Tests pass when run individually but fail in batch
- Test order matters (Test A fails if Test B runs first)
- Mysterious failures with "expected nil but got X"

**Example of Flaky Test:**

```elisp
;; ❌ Global state shared between tests
(defvar test-shared-buffer nil)

(def-sly-test flaky-setup ()
  (setq test-shared-buffer (generate-new-buffer "*test*"))
  (with-current-buffer test-shared-buffer
    (insert "data")))

(def-sly-test flaky-use ()
  ;; Fails if flaky-setup didn't run first!
  (sly-check "Buffer exists"
    (buffer-live-p test-shared-buffer)))
```

**Solution: Isolate Test State**

```elisp
;; ✅ Each test manages its own state
(def-sly-test robust-test ()
  (let ((test-buffer (generate-new-buffer "*test*")))
    (unwind-protect
        (with-current-buffer test-buffer
          (insert "data")
          (sly-check "Buffer has data"
            (= 4 (buffer-size))))
      ;; Always clean up
      (when (buffer-live-p test-buffer)
        (kill-buffer test-buffer)))))
```

**Key Points:**
- Never use global variables to share state between tests
- Always use `unwind-protect` to ensure cleanup
- Each test should be completely independent

## Environment Dependencies

### Problem: Tests Assume Specific Configuration

**Symptoms:**
- Tests fail for some users but not others
- Tests fail with different Emacs versions
- Tests fail with different Common Lisp implementations

**Example of Flaky Test:**

```elisp
(def-sly-test flaky-user-config ()
  "Flaky test assuming user configuration."
  ;; ❌ Assumes user has this package installed
  (require 'company)
  (sly-check "Company completion works"
    (sly-company-active-p)))
```

**Solution: Explicit Test Setup**

```elisp
(def-sly-test robust-optional-feature ()
  "Robust test with optional dependency."
  ;; ✅ Gracefully handle optional features
  (if (require 'company nil t)
      (sly-check "Company completion works"
        (sly-company-active-p))
    (message "Skipping company test - not installed")))
```

**Better Solution: Feature Flags**

```elisp
(def-sly-test robust-optional-feature-v2 ()
  "Test with proper feature detection."
  :tags '(requires-company)  ; Tag for selective running
  (unless (featurep 'company)
    (ert-skip "Company not available"))
  (sly-check "Company completion works"
    (sly-company-active-p)))
```

**Key Points:**
- Don't assume packages beyond SLY's dependencies are available
- Use `ert-skip` for tests requiring optional features
- Tag tests that have special requirements

## Resource Management

### Problem: Tests Leak Resources

**Symptoms:**
- Memory usage grows during test runs
- "Too many open files" errors
- Buffers or processes left behind after tests

**Example of Flaky Test:**

```elisp
(def-sly-test flaky-resource-leak ()
  "Flaky test that leaks buffers."
  (dotimes (i 100)
    (let ((buf (generate-new-buffer (format "*test-%d*" i))))
      ;; ❌ Buffers never killed
      (with-current-buffer buf
        (insert "data")
        (sly-check "Buffer created" t)))))
```

**Solution: Proper Resource Cleanup**

```elisp
(def-sly-test robust-resource-management ()
  "Robust test with cleanup."
  (dotimes (i 100)
    (let ((buf (generate-new-buffer (format "*test-%d*" i))))
      (unwind-protect
          (with-current-buffer buf
            (insert "data")
            (sly-check "Buffer created" t))
        ;; ✅ Always clean up
        (when (buffer-live-p buf)
          (kill-buffer buf))))))
```

**For Complex Resources:**

```elisp
(defmacro with-test-resources (&rest body)
  "Execute BODY with automatic resource cleanup."
  (declare (indent 0))
  (let ((buffers (cl-gensym "buffers"))
        (overlays (cl-gensym "overlays")))
    `(let ((,buffers nil)
           (,overlays nil))
       (cl-flet ((track-buffer (buf)
                   (push buf ,buffers)
                   buf)
                 (track-overlay (ov)
                   (push ov ,overlays)
                   ov))
         (unwind-protect
             (progn ,@body)
           ;; Cleanup
           (mapc #'kill-buffer
                 (cl-remove-if-not #'buffer-live-p ,buffers))
           (mapc #'delete-overlay ,overlays))))))
```

**Key Points:**
- Always use `unwind-protect` for cleanup
- Track all created resources (buffers, overlays, timers, processes)
- Clean up even when tests fail

## Async Operations

### Problem: Callbacks Not Invoked Yet

**Symptoms:**
- Tests fail with "callback not called"
- Tests pass sometimes (when machine is fast)
- Tests fail under debugger (timing changes)

**Example of Flaky Test:**

```elisp
(def-sly-test flaky-async ()
  "Flaky async test."
  (let ((result nil))
    (sly-eval-async '(slynk:connection-info)
      (lambda (info)
        (setq result info)))
    ;; ❌ Result might not be set yet
    (sly-check "Got result"
      result)))
```

**Solution: Wait for Callback**

```elisp
(def-sly-test robust-async ()
  "Robust async test."
  (let ((result nil)
        (callback-called nil))
    (sly-eval-async '(slynk:connection-info)
      (lambda (info)
        (setq result info
              callback-called t)))
    ;; ✅ Wait for callback
    (sly-wait-condition "Callback invoked" 5.0
      callback-called)
    (sly-check "Got result"
      result)
    (sly-check "Result has expected structure"
      (plist-get result :pid))))
```

**Key Points:**
- Never check async results immediately
- Use flags to track callback invocation
- Use `sly-wait-condition` to wait for flags

## Prevention Checklist

Before merging a test, verify:

### Timing
- [ ] No `sleep-for` or fixed delays
- [ ] Uses `sly-wait-condition` for async operations
- [ ] Uses `sly-sync-to-top-level` after eval
- [ ] Timeout values are generous (≥3 seconds)

### Independence
- [ ] Test doesn't modify global state
- [ ] Test doesn't depend on other tests
- [ ] Test cleans up all resources in `unwind-protect`
- [ ] Test can run in any order

### Determinism
- [ ] Test doesn't use random values without seeding
- [ ] Test doesn't depend on current time/date
- [ ] Test doesn't depend on file system state
- [ ] Test doesn't depend on network availability

### Resource Management
- [ ] All buffers are killed
- [ ] All overlays are deleted
- [ ] All timers are canceled
- [ ] All processes are killed
- [ ] All temp files are deleted

### Environment
- [ ] Test doesn't assume specific Emacs configuration
- [ ] Test doesn't assume specific OS
- [ ] Test works with both SBCL and CCL
- [ ] Test doesn't require optional packages without checking

## Debugging Flaky Tests

### Technique 1: Run in Loop

```bash
# Run test 100 times to expose flakiness
for i in {1..100}; do
  echo "Run $i"
  make check-unit || break
done
```

### Technique 2: Add Instrumentation

```elisp
(def-sly-test debug-flaky ()
  "Instrumented test."
  (let ((start (current-time)))
    (sly-eval-async '(slynk:connection-info)
      (lambda (info)
        (message "Callback after %.3fs"
                 (float-time (time-since start)))))
    (sly-sync-to-top-level 10)
    ;; Check timing in *Messages* buffer
    ))
```

### Technique 3: Enable Event Logging

```elisp
(setq sly-log-events t)
;; View *sly-events* buffer to see all RPC traffic
```

### Technique 4: Bisect Test Suite

```bash
# Find which test causes state corruption
# Run tests in different orders
make check-unit SELECTOR='(or (tag test-group-a) (tag test-group-b))'
```

## Real-World Examples

### Example 1: REPL Output Timing

**Before (Flaky):**
```elisp
(def-sly-test flaky-repl-output-example ()
  (sly-eval '(+ 1 2))
  (sleep-for 0.2)
  (sly-check "Output appeared" (looking-at "3")))
```

**After (Robust):**
```elisp
(def-sly-test robust-repl-output-example ()
  (sly-eval '(+ 1 2))
  (sly-sync-to-top-level 3)
  (sly-check "Output appeared"
    (save-excursion
      (goto-char (point-min))
      (re-search-forward "^3$" nil t))))
```

### Example 2: Buffer Creation Race

**Before (Flaky):**
```elisp
(def-sly-test flaky-inspector-example ()
  (sly-inspect '(list 1 2 3))
  (sleep-for 0.5)
  (sly-check "Inspector buffer exists"
    (get-buffer "*sly-inspector*")))
```

**After (Robust):**
```elisp
(def-sly-test robust-inspector-example ()
  (sly-inspect '(list 1 2 3))
  (sly-sync-to-top-level 3)
  (sly-wait-condition "Inspector buffer created" 3.0
    (get-buffer "*sly-inspector*"))
  (sly-check "Inspector shows list"
    (with-current-buffer "*sly-inspector*"
      (buffer-contains-p "LIST"))))
```

## Summary

**Golden Rules for Non-Flaky Tests:**

1. **Never use fixed delays** - Always wait for actual conditions
2. **Always clean up** - Use `unwind-protect` for all resources
3. **Tests must be independent** - No shared global state
4. **Be patient** - Use generous timeouts (seconds, not milliseconds)
5. **Test what matters** - Wait for the actual state change, not time elapsed

Following these patterns will make your tests reliable across all environments and CI systems.

## Additional Resources

- [CONTRIBUTING.md](../CONTRIBUTING.md#testing) - Main testing guide
- [TESTING-CONTRIBS.md](TESTING-CONTRIBS.md) - Contrib testing guide
- [lib/sly-tests.el](../lib/sly-tests.el) - Test framework with `sly-wait-condition`
- [GitHub Issues](https://github.com/joaotavora/sly/issues?q=is%3Aissue+flaky+test) - Historical flaky test issues
