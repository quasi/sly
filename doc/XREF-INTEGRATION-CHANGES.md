# SLY xref.el Integration - Change Report

**Date:** 2026-01-02
**Status:** Complete

## Summary

This document records the changes made to integrate SLY with Emacs's built-in `xref.el` framework. The integration follows a "soft deprecation" strategy: old commands remain functional while new xref.el integration is added.

## Files Changed

### New Files Created

| File | Purpose |
|------|---------|
| `lib/sly-xref.el` | xref.el backend implementation |
| `test/sly-xref-unit-tests.el` | Unit tests for xref integration |
| `doc/XREF-COMPARISON.md` | Comparison of SLY vs xref.el architectures |
| `doc/plans/2026-01-02-xref-integration-design.md` | Design document |
| `doc/XREF-INTEGRATION-CHANGES.md` | This change report |

### Modified Files

| File | Changes |
|------|---------|
| `sly.el` | Added require for sly-xref.el; added soft-deprecation notes to xref functions |
| `Makefile` | Added sly-xref-unit-tests to check-unit target |
| `test/sly-common-unit-tests.el` | Fixed syntax error (unrelated) |

## Implementation Details

### 1. xref.el Backend (`lib/sly-xref.el`)

Created a new backend that registers with `xref-backend-functions`:

```elisp
(defun sly-xref-backend ()
  "SLY backend for xref.el."
  (when (and (fboundp 'sly-connected-p) (sly-connected-p))
    'sly))

(add-hook 'xref-backend-functions #'sly-xref-backend)
```

**Backend Methods Implemented:**

| Method | Description |
|--------|-------------|
| `xref-backend-identifier-at-point` | Returns symbol at point via `sly-symbol-at-point` |
| `xref-backend-definitions` | Finds definitions via `slynk:find-definitions-for-emacs` |
| `xref-backend-references` | Finds callers via `slynk:xref :callers` |
| `xref-backend-apropos` | Pattern search via `slynk:apropos-list-for-emacs` |
| `xref-backend-identifier-completion-table` | Symbol completion |

### 2. Location Conversion

The core challenge was converting between Slynk's location format:

```lisp
(:location (:file "/path") (:position 1234) (:snippet "..."))
```

And xref.el's expected format:

```elisp
(xref-make-file-location FILE LINE COLUMN)
```

**Key functions:**

- `sly-xref--convert-to-xref-items`: Converts list of Slynk xrefs to xref-items
- `sly-xref--convert-location`: Handles various Slynk location types
- `sly-xref--position-to-line-col`: Converts byte positions to line/column
- `sly-xref--resolve-position-in-file`: Resolves different position spec types

**Optimization:** For `:line` position specs, line/column are returned directly without reading the file.

### 3. Unified Query Command

Added `sly-xref-query` for accessing all Slynk xref types:

```elisp
(defvar sly-xref-query-types
  '(("calls (who calls this)" . :calls)
    ("calls-who (called by this)" . :calls-who)
    ("references (who references this)" . :references)
    ("binds (who binds this)" . :binds)
    ("sets (who sets this)" . :sets)
    ("macroexpands (who expands this)" . :macroexpands)
    ("specializes (methods on this class)" . :specializes)
    ("callers (portable who-calls)" . :callers)
    ("callees (portable calls-who)" . :callees)))
```

Usage: `M-x sly-xref-query` prompts for type and symbol.

### 4. Recompile Minor Mode

Added `sly-xref-recompile-mode` for xref buffers:

| Keybinding | Command | Description |
|------------|---------|-------------|
| `C-c C-c` | `sly-xref-recompile-at-point` | Recompile definition at point |
| `C-c C-k` | `sly-xref-recompile-all` | Recompile all definitions |

### 5. Backward Compatibility

All existing commands in `sly.el` remain functional with soft-deprecation notes:

- `sly-edit-definition` - Note: prefer `xref-find-definitions` (M-.)
- `sly-edit-uses` - Note: prefer `xref-find-references` (M-?) or `sly-xref-query`
- `sly-who-calls` and related - Continue to work via `sly-xref`

## Keybinding Changes

| Binding | Old Command | New Behavior |
|---------|-------------|--------------|
| `M-.` | `sly-edit-definition` | Now handled by `xref-find-definitions` when connected |
| `M-,` | `sly-pop-find-definition-stack` | Now handled by `xref-go-back` |
| `M-?` | `sly-edit-uses` | Now handled by `xref-find-references` |

## Test Coverage

Added 24 unit tests covering:

| Category | Tests |
|----------|-------|
| Backend registration | 2 |
| Location conversion | 7 |
| Position resolution | 4 |
| Query system | 3 |
| Backward compatibility | 2 |
| Recompile mode | 1 |
| Backend methods | 5 |

All 165 unit tests pass (141 existing + 24 new).

## Design Decisions

| Decision | Choice | Rationale |
|----------|--------|-----------|
| Backward compatibility | Soft deprecation | Minimize disruption; gradual migration path |
| Async handling | Synchronous | Simpler implementation; Slynk queries are fast (<100ms) |
| Extended xref types | Unified `sly-xref-query` | Single entry point; displays in standard `*xref*` buffer |
| Recompile feature | Minor mode | Preserves batch-recompile functionality in xref buffers |
| Multiple connections | Current connection only | Matches existing behavior; simpler implementation |

## Column Handling Note

xref.el uses 0-based columns while Slynk uses 1-based. The conversion automatically adjusts:

```elisp
;; For :line specs, convert 1-based to 0-based
(cons line (if col (1- col) 0))
```

## What This Enables

1. **Standard Navigation:** `M-.`, `M-,`, `M-?` work identically to other Emacs modes
2. **Integration:** Works with `project.el`, `grep.el`, and other Emacs infrastructure
3. **Consistency:** Results appear in standard `*xref*` buffer
4. **Extended Queries:** All Slynk xref types via unified `sly-xref-query`
5. **Recompilation:** Batch recompile from xref results buffer

## Migration Path for Users

**Immediate:** No action required. Existing workflows continue to work.

**Recommended:**
- Use `M-.` instead of `M-x sly-edit-definition`
- Use `M-?` instead of `M-x sly-edit-uses`
- Use `M-x sly-xref-query` for advanced xref types

## References

- [Emacs xref.el documentation](https://www.gnu.org/software/emacs/manual/html_node/emacs/Xref.html)
- [Design document](plans/2026-01-02-xref-integration-design.md)
- [Comparison report](XREF-COMPARISON.md)
