# SLY xref vs Emacs xref.el: Comparison Report

This document compares SLY's bespoke cross-reference system with Emacs's built-in `xref.el` framework, analyzing differences and documenting the rationale for migrating to the standard Emacs API.

## Executive Summary

SLY inherited its xref system from SLIME, which predates Emacs's `xref.el` (introduced in Emacs 25.1). While SLY's system is functional, migrating to `xref.el` provides:

1. **Consistency**: Standard keybindings and UI patterns familiar to Emacs users
2. **Integration**: Works with `project.el`, `grep.el`, and other Emacs infrastructure
3. **Maintenance**: Reduced code to maintain; leverages Emacs core improvements
4. **Future-proofing**: Other packages increasingly expect xref.el integration

## Architecture Comparison

### SLY's Current System

```
┌─────────────────────────────────────────────────────────────────┐
│                        Emacs Side (sly.el)                      │
├─────────────────────────────────────────────────────────────────┤
│  sly-edit-definition          → sly-eval '(slynk:find-...)     │
│  sly-who-calls                → sly-eval '(slynk:xref ...)     │
│  sly-xref-mode                → Custom buffer display           │
│  sly-xref--show-results       → Custom results rendering        │
│  sly-push-definition-stack    → xref-push-marker-stack (compat) │
└─────────────────────────────────────────────────────────────────┘
                                  │
                                  ▼ RPC over TCP
┌─────────────────────────────────────────────────────────────────┐
│                     Slynk Side (slynk.lisp)                     │
├─────────────────────────────────────────────────────────────────┤
│  find-definitions-for-emacs   → Returns ((DSPEC LOCATION) ...)  │
│  xref (type name)             → Dispatches to xref-doit        │
│  xrefs (types name)           → Multiple xref queries          │
│  xref-doit                    → Generic function by type        │
└─────────────────────────────────────────────────────────────────┘
                                  │
                                  ▼ Backend interface
┌─────────────────────────────────────────────────────────────────┐
│                   slynk-backend.lisp (interface)                │
├─────────────────────────────────────────────────────────────────┤
│  find-definitions             → Per-implementation              │
│  who-calls, calls-who         → Compiler-specific               │
│  who-references, who-binds    → Compiler-specific               │
│  who-sets, who-macroexpands   → Compiler-specific               │
│  who-specializes              → CLOS introspection              │
│  list-callers, list-callees   → Portable fallback (xref.lisp)  │
└─────────────────────────────────────────────────────────────────┘
```

### Emacs xref.el Architecture

```
┌─────────────────────────────────────────────────────────────────┐
│                         xref.el (Emacs core)                    │
├─────────────────────────────────────────────────────────────────┤
│  xref-find-definitions        → M-.  standard binding          │
│  xref-find-references         → M-?  standard binding          │
│  xref-go-back                 → M-,  standard binding          │
│  xref-backend-functions       → Hook for backend discovery     │
│  *xref* buffer                → Standardized results display    │
└─────────────────────────────────────────────────────────────────┘
                                  │
                                  ▼ Backend protocol
┌─────────────────────────────────────────────────────────────────┐
│                      Backend Methods (cl-defmethod)             │
├─────────────────────────────────────────────────────────────────┤
│  xref-backend-identifier-at-point  → Get symbol at point       │
│  xref-backend-definitions          → Find definitions          │
│  xref-backend-references           → Find references           │
│  xref-backend-apropos              → Pattern-based search      │
│  xref-backend-identifier-completion-table → Completion         │
└─────────────────────────────────────────────────────────────────┘
                                  │
                                  ▼ Data structures
┌─────────────────────────────────────────────────────────────────┐
│                         xref Objects                            │
├─────────────────────────────────────────────────────────────────┤
│  xref-item                    → (summary . location)           │
│  xref-file-location           → (file line column)             │
│  xref-buffer-location         → (buffer position)              │
│  xref-bogus-location          → Error placeholder              │
└─────────────────────────────────────────────────────────────────┘
```

## Feature Mapping

| SLY Feature                | xref.el Equivalent                    | Notes                              |
|---------------------------|---------------------------------------|-----------------------------------|
| `sly-edit-definition`     | `xref-find-definitions`              | Direct mapping                    |
| `sly-who-calls`           | `xref-find-references` + filtering   | Needs type parameter              |
| `sly-edit-uses`           | `xref-find-references`               | Combined query                    |
| `sly-xref-mode`           | `xref--xref-buffer-mode`             | Standard buffer                   |
| `sly-xref--show-results`  | `xref--show-xrefs`                   | Internal function                 |
| `sly-push-definition-stack`| `xref-push-marker-stack`            | Already used as fallback          |
| `sly-pop-find-definition-stack`| `xref-go-back`                  | Standard navigation               |
| `sly-xref` struct         | `xref-item` class                    | Similar structure                 |
| `sly-location` struct     | `xref-file-location` class           | Similar structure                 |

## Data Structure Mapping

### SLY Location Format (from Slynk)

```lisp
;; Slynk returns locations in this format:
(:location
  (:file "/path/to/file.lisp")    ; or (:buffer "buffer-name")
  (:position 1234)                 ; byte offset
  (:snippet "code context"))       ; optional
```

### xref.el Location Objects

```elisp
;; xref.el expects these objects:
(xref-make-file-location FILE LINE COLUMN)
(xref-make-buffer-location BUFFER POINT)
(xref-make-bogus-location MESSAGE)
```

### Conversion Required

The main conversion work is transforming Slynk's `(:location ...)` format to xref location objects. This requires:

1. Converting byte positions to line/column (for `xref-file-location`)
2. Handling buffer locations (for `xref-buffer-location`)
3. Converting error locations (`:error "message"`) to `xref-bogus-location`

## SLY-Specific Features Not in xref.el

SLY has several xref features beyond simple definition/reference lookup:

| SLY Feature           | xref.el Support | Implementation Strategy            |
|----------------------|-----------------|-----------------------------------|
| `:calls`             | Via references  | Filter by call-site type          |
| `:calls-who`         | No equivalent   | Custom menu/command               |
| `:references`        | Supported       | Direct mapping                    |
| `:binds`             | No equivalent   | Custom menu/command               |
| `:sets`              | No equivalent   | Custom menu/command               |
| `:macroexpands`      | No equivalent   | Custom menu/command               |
| `:specializes`       | No equivalent   | Custom menu/command               |
| `:callers`           | Via references  | Direct mapping                    |
| `:callees`           | No equivalent   | Custom menu/command               |
| Recompile from xref  | No equivalent   | Custom buffer command             |

### Recommended Approach

1. **Core integration**: Map `sly-edit-definition` → `xref-find-definitions`
2. **References**: Map `sly-who-calls` and similar → `xref-find-references` with type annotation
3. **Extended features**: Keep SLY-specific commands that call xref.el infrastructure but display in enhanced buffer
4. **Recompilation**: Add to xref buffer via major-mode customization

## Keybinding Analysis

### Current SLY Bindings

```elisp
(define-key map (kbd "M-.")     'sly-edit-definition)
(define-key map (kbd "M-,")     'sly-pop-find-definition-stack)
(define-key map (kbd "M-_")     'sly-edit-uses)    ; German layout
(define-key map (kbd "M-?")     'sly-edit-uses)    ; US layout
(define-key map (kbd "C-x 4 .") 'sly-edit-definition-other-window)
(define-key map (kbd "C-x 5 .") 'sly-edit-definition-other-frame)
```

### Standard xref.el Bindings

```elisp
M-.       xref-find-definitions
M-,       xref-go-back
M-?       xref-find-references
C-x 4 .   xref-find-definitions-other-window
C-x 5 .   xref-find-definitions-other-frame
```

### Compatibility Note

SLY's bindings are already aligned with xref.el conventions. The migration should be transparent to users for the primary navigation commands.

## Implementation Complexity

### Low Complexity
- Backend registration via `xref-backend-functions`
- `xref-backend-identifier-at-point` implementation
- `xref-backend-definitions` implementation
- Location conversion logic

### Medium Complexity
- `xref-backend-references` with type filtering
- Maintaining SLY's extended xref types (binds, sets, etc.)
- Buffer/position to line/column conversion

### High Complexity
- Preserving recompilation functionality in xref buffer
- Async result handling (xref.el expects synchronous returns by default)
- Multiple connections (which Slynk to query?)

## Async Considerations

SLY's RPC is inherently asynchronous (`sly-eval-async`), but xref.el's backend methods expect synchronous returns. Options:

1. **Blocking call**: Use `sly-eval` (synchronous) instead of `sly-eval-async`
   - Simpler implementation
   - May cause UI freezes for slow queries

2. **Promise-based**: Return immediately with placeholder, update when ready
   - Requires xref.el modification or workaround
   - Better UX for large codebases

3. **Hybrid**: Synchronous for small results, async with progress for large
   - Most complex to implement
   - Best user experience

**Recommendation**: Start with synchronous calls. The Slynk queries are typically fast (<100ms). Optimize if performance issues arise.

## Risks and Mitigations

| Risk                                  | Mitigation                                           |
|--------------------------------------|-----------------------------------------------------|
| Breaking existing user workflows     | Maintain command aliases for compatibility           |
| Loss of SLY-specific features        | Keep extended commands, integrate with xref UI      |
| Async/sync mismatch                  | Use synchronous RPC initially                        |
| Position conversion errors           | Comprehensive test suite for edge cases              |
| Multiple connection handling         | Use `sly-current-connection` consistently           |

## Conclusion

Migrating SLY to use Emacs's `xref.el` is feasible and beneficial. The core functionality maps directly, and SLY-specific features can be layered on top. The main work involves:

1. Implementing the xref backend methods
2. Converting location formats
3. Handling async RPC (initially via synchronous calls)
4. Maintaining extended xref types as SLY-specific commands

The migration reduces maintenance burden and improves integration with the broader Emacs ecosystem.

## References

- [Emacs xref.el source](https://github.com/emacs-mirror/emacs/blob/master/lisp/progmodes/xref.el)
- [xref-js2 implementation example](https://github.com/NicolasPetton/xref-js2/blob/master/xref-js2.el)
- [ESS-R xref backend](https://github.com/emacs-ess/ESS/blob/master/lisp/ess-r-xref.el)
- [GNU Emacs xref documentation](https://www.gnu.org/software/emacs/manual/html_node/emacs/Xref.html)
- [SLIME xref implementation](https://github.com/slime/slime/blob/master/slime.el)
