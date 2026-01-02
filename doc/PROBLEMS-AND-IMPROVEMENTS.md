# SLY: Problems and Improvement Opportunities

This document provides a critical analysis of SLY's codebase, identifying architectural issues, technical debt, and opportunities for improvement. It is intended for developers considering contributing to SLY or understanding its limitations.

---

## Executive Summary

SLY is a mature, functional IDE that has evolved organically from SLIME since 2014. While it works well for most users, the codebase shows signs of age:

- **Monolithic core** - 7,500+ lines in a single file
- **Technical debt** - Many FIXMEs and TODOs acknowledged but not addressed
- **Inconsistent abstractions** - Some areas are over-engineered, others under-abstracted
- **Limited test coverage** - Many complex features lack tests
- **Documentation gaps** - Protocol and internals poorly documented (until now)

---

## 1. Architectural Issues

### 1.1 Monolithic sly.el

**Problem:** The main `sly.el` file contains 7,511 lines covering:
- Connection management
- RPC layer
- Compilation
- Evaluation
- Definition navigation
- Documentation
- Macroexpansion
- Debugger (SLY-DB)
- Inspector
- Contrib system
- Utilities

**Impact:**
- Difficult to navigate and understand
- Changes in one area can unexpectedly affect others
- Load time increased by loading everything
- Testing individual components is harder

**Recommendation:** Split into focused modules:
```
sly-core.el        # Fundamental types and utilities
sly-connection.el  # Connection management
sly-rpc.el         # RPC layer
sly-compile.el     # Compilation
sly-eval.el        # Evaluation
sly-debug.el       # SLY-DB
sly-inspect.el     # Inspector
sly-xref.el        # Cross-references
```

### 1.2 Global State Management

**Problem:** Heavy reliance on global variables and dynamic binding:

```elisp
;; Global connection state
(defvar sly-net-processes nil)
(defvar sly-default-connection nil)

;; Buffer-local connection (easily out of sync)
(defvar-local sly-buffer-connection nil)

;; Implicit global state
(defvar sly-rex-continuations nil)  ; Per-connection but accessed globally
```

**Impact:**
- Race conditions with multiple connections
- Confusing to determine which connection a buffer uses
- Makes testing difficult

**Recommendation:**
- Encapsulate connection state in explicit objects
- Pass connections explicitly rather than relying on dynamic scope
- Consider using Emacs 29+ `slot` for structured data

### 1.3 Protocol Underspecification

**Problem:** The SLYNK protocol is not formally specified. Message types and their semantics are only discoverable by reading code.

```elisp
;; From sly.el - protocol is embedded in dispatch logic
(sly-dcase event
  ((:return value id) ...)
  ((:debug thread level condition restarts frames conts) ...)
  ;; etc - 25+ message types
```

**Impact:**
- Difficult to implement alternative clients
- Protocol changes can break compatibility silently
- No way to validate messages

**Recommendation:**
- Create formal protocol specification (now done in SLYNK-PROTOCOL.md)
- Consider adding protocol version negotiation
- Add message validation on both ends

### 1.4 Threading Model Complexity

**Problem:** SLYNK supports four different communication styles with different threading models:

```lisp
(defvar *communication-style* :spawn
  "Style of communication: :spawn, :sigio, :fd-handler, or nil")
```

Each style requires different code paths throughout the server.

**Impact:**
- Code duplication and conditional branches everywhere
- Some styles are poorly tested
- Bugs may appear in one style but not others

**Recommendation:**
- Consider dropping rarely-used styles (:sigio, :fd-handler)
- Or abstract differences behind a cleaner interface

---

## 2. Technical Debt

### 2.1 Documented FIXMEs and TODOs

The codebase contains 50+ FIXME/TODO comments. Key ones:

| Location | Issue |
|----------|-------|
| sly.el:142 | Contrib setup contract unclear |
| sly.el:2841 | Dead code suspected |
| sly.el:3037 | Questionable design |
| sly.el:3328-3329 | Messy code needs cleanup |
| sly.el:4554 | Unfinished feature |
| sly.el:4991 | Recompilation cruft |
| slynk.lisp:721 | Overdesigned |
| slynk.lisp:1590 | Not thread safe |
| slynk.lisp:2279 | 45 lines for simple task |
| lib/sly-parse.el:72 | Should use syntax-ppss |

### 2.2 Backward Compatibility Cruft

**Problem:** Code maintains compatibility with very old Emacs versions:

```elisp
(eval-and-compile
  (if (version< emacs-version "24.5")
      (error "Sly requires at least Emacs 24.5")))
```

Emacs 24.5 was released in 2015. SLY could safely require Emacs 27+ now.

**Impact:**
- Cannot use modern Emacs features
- Extra code for compatibility shims
- Workarounds for bugs fixed in newer Emacs

**Recommendation:** Bump minimum version to Emacs 27.1 (2020) or 28.1 (2022).

### 2.3 Dead or Orphaned Code

**Problem:** Comments suggest dead code exists:

```elisp
;; sly.el:2841
;; FIXME: I doubt that anybody uses this directly and it seems to be
```

**Recommendation:** Audit and remove unused functions.

### 2.4 Copy-Paste Duplication

**Problem:** Similar patterns repeated across contribs:

```elisp
;; In multiple contribs:
(:on-load
 (add-hook 'sly-connected-hook '...)
 (add-hook 'sly-mode-hook '...))
(:on-unload
 (remove-hook 'sly-connected-hook '...)
 (remove-hook 'sly-mode-hook '...))
```

**Recommendation:** Create macros/helpers for common contrib patterns.

---

## 3. Security Issues

### 3.1 Network Exposure

**From PROBLEMS.md:**
> The `M-x sly` command has Lisp listen on a TCP socket... If someone else were to connect to this socket then they could use the SLY protocol to control the Lisp process.

**Current mitigations:**
- Bind to loopback only
- Optional secret file (`~/.sly-secret`)

**Remaining issues:**
- Secret is transmitted in plaintext
- No TLS/encryption support
- No authentication handshake timeout (fixed but implementation varies)

**Recommendation:**
- Document security best practices prominently
- Consider adding TLS support for non-localhost connections
- Add proper authentication with timeout by default

### 3.2 Code Execution Risk

SLYNK allows arbitrary code execution by design. Any client that can connect can:
- Execute arbitrary Lisp code
- Read/write files
- Kill processes
- Access network

**Recommendation:**
- Consider adding sandboxing options
- Allow restricting available operations
- Log all remote evaluations

---

## 4. Usability Issues

### 4.1 Error Messages

**Problem:** Error messages often lack context:

```elisp
(error "No SLY connection")
```

**Better:**
```elisp
(error "No SLY connection for buffer %s. Use M-x sly to start one."
       (buffer-name))
```

### 4.2 Startup Complexity

**Problem:** Multiple ways to start SLY with different behaviors:

- `M-x sly` - Start Lisp, connect
- `M-x sly-connect` - Connect to running server
- `sly-setup` - Configure contribs

Users often confused about which to use.

**Recommendation:**
- Simplify startup flow
- Add guided setup wizard
- Better first-run experience

### 4.3 Keybinding Conflicts

**Problem:** SLY uses `C-c` prefix extensively, conflicting with:
- `comint-mode` bindings
- User customizations
- Other packages

**Recommendation:**
- Document all keybindings in one place
- Provide easy customization via keymap variable
- Consider using `C-c s` prefix for all SLY commands

---

## 5. Backend-Specific Issues

### 5.1 Implementation Matrix Gaps

From PROBLEMS.md and code analysis:

| Issue | Affected Backends |
|-------|-------------------|
| Threading limitations | CLISP |
| Weak debugging | ABCL, ECL, CLISP |
| Slow interrupts | Allegro CL |
| Unicode issues | LispWorks |
| Hangs on Windows | LispWorks |
| Arglist names wrong | CLISP |
| `READ-CHAR-NO-HANG` broken | All |
| Source location granularity | SBCL (without debug 2) |

### 5.2 Backend Code Duplication

**Problem:** Each backend reimplements similar functionality:

```lisp
;; Same pattern in sbcl.lisp, ccl.lisp, cmucl.lisp, etc.
(defimplementation frame-source-location (index)
  ;; Implementation-specific code to find source location
  ...)
```

Some backends have 2000+ lines with significant duplication.

**Recommendation:**
- Extract common patterns into slynk-backend.lisp
- Provide default implementations where possible
- Create shared test suite for backends

---

## 6. Testing Deficiencies

### 6.1 Coverage Gaps

**Tested:**
- Basic connection
- Evaluation
- Some completion
- Indentation

**Untested or undertested:**
- Multi-connection scenarios
- Debugger (SLY-DB) interactions
- Inspector
- Stickers
- Trace dialog
- Error recovery
- Threading edge cases

### 6.2 Test Infrastructure Issues

```elisp
;; lib/sly-tests.el:1314
;; FIXME: suboptimal: wait one second for the lisp

;; lib/sly-tests.el:1144
;; FIXME: sly-wait-condition returns immediately if the test returns true
```

**Problems:**
- Flaky timing-dependent tests
- No CI matrix for multiple Lisp implementations
- Tests require manual setup

### 6.3 Recommendations

- Add integration tests for major features
- Set up CI for multiple backends (SBCL, CCL, ECL at minimum)
- Add property-based testing for protocol
- Create mock server for Emacs-side testing

---

## 7. Documentation Deficiencies

### 7.1 Current State

- `sly.texi` - User manual (good but incomplete)
- `PROBLEMS.md` - Known issues (brief)
- Code comments - Inconsistent
- API documentation - Minimal

### 7.2 Missing Documentation

- Protocol specification (now created)
- Architecture overview (now created)
- Contrib development guide (now created)
- Backend implementation guide
- Debugging/troubleshooting guide

### 7.3 Recommendations

- ✅ Create protocol documentation (SLYNK-PROTOCOL.md)
- ✅ Create architecture documentation (SLY-ARCHITECTURE.md)
- ✅ Create contrib guide (CONTRIB-ARCHITECTURE.md)
- Add inline documentation standards
- Generate API reference from docstrings

---

## 8. Modernization Opportunities

### 8.1 Emacs Features Not Used

SLY could benefit from modern Emacs features:

| Feature | Since | Benefit |
|---------|-------|---------|
| `seq.el` | 25.1 | Cleaner sequence operations |
| `map.el` | 25.1 | Cleaner map operations |
| `subr-x.el` | 24.4 | String utilities |
| Native JSON | 27.1 | Faster parsing |
| Native threads | 26.1 | Background operations |
| Project.el | 26.1 | Project detection |
| Tab-bar | 27.1 | UI organization |
| `shortdoc.el` | 28.1 | Function groups |

### 8.2 LSP Integration

**Current state:** SLY predates LSP and uses its own protocol.

**Opportunity:** Create LSP adapter for:
- IDE integration (VSCode, etc.)
- Standard tooling
- Wider ecosystem

**Challenge:** SLYNK protocol is richer than LSP for Lisp-specific features.

### 8.3 Tree-sitter Support

Emacs 29 includes tree-sitter. Benefits:
- Faster, more accurate parsing
- Better syntax highlighting
- Improved navigation

**Challenge:** No Common Lisp grammar exists for tree-sitter yet.

---

## 9. Performance Issues

### 9.1 Completion Latency

**Problem:** Completion can be slow with large symbol sets.

```elisp
;; lib/sly-completion.el:375
;;; TODO: Most of the stuff emulates `completion--in-region'
```

**Recommendation:**
- Implement incremental completion
- Cache completion results
- Use scoring/ranking to limit results

### 9.2 Large Output Handling

**Problem:** Large output from Lisp can freeze Emacs.

**Recommendation:**
- Implement output pagination
- Add configurable limits
- Stream output asynchronously

### 9.3 Connection Startup

**Problem:** Initial connection setup loads many modules.

**Recommendation:**
- Lazy-load less common features
- Profile and optimize startup sequence
- Add connection caching

---

## 10. Contrib-Specific Issues

### 10.1 sly-stickers

**Issues:**
- Zombie sticker tracking is complex
- Instrumentation can affect performance
- Limited to compile-time annotation

### 10.2 sly-trace-dialog

**Issues:**
- Large traces consume memory
- No export format
- Tree rendering slow for deep traces

### 10.3 sly-mrepl

**Issues:**
- History management could be better
- No persistent history across sessions
- Input editing limited compared to eshell

---

## 11. Improvement Roadmap

### Short-term (Low effort, high impact)

1. ✅ Document protocol (completed)
2. ✅ Document architecture (completed)
3. Fix critical FIXMEs (20+ identified)
4. Bump minimum Emacs version
5. Improve error messages

### Medium-term (Moderate effort)

1. Split sly.el into focused modules
2. Add comprehensive tests
3. Set up CI for multiple backends
4. Modernize using Emacs 27+ features
5. Document backend implementation

### Long-term (Significant effort)

1. LSP adapter
2. Tree-sitter support
3. TLS/security improvements
4. Performance optimization
5. Protocol versioning

---

## 12. Conclusion

SLY is a powerful, well-designed IDE that has served the Common Lisp community well. Its core architecture is sound, but a decade of evolution has left technical debt. The main priorities for improvement are:

1. **Documentation** - Making the codebase accessible to new contributors
2. **Testing** - Ensuring reliability across configurations
3. **Modernization** - Using current Emacs capabilities
4. **Modularization** - Breaking up the monolith

The codebase is maintainable and the original design decisions (protocol, contrib system, backend abstraction) remain valid. With targeted investment, SLY can continue to be the premier Common Lisp IDE for another decade.

---

## Appendix: FIXME/TODO Index

| File | Line | Issue |
|------|------|-------|
| sly.el | 142 | Contract should be like hypothetical sly-refresh-contribs |
| sly.el | 209 | Forward reference to sly-editing-mode |
| sly.el | 1373 | load-server & start-server separation |
| sly.el | 2077 | Sync with sly-buffer-name |
| sly.el | 2540 | Force inhibit-quit |
| sly.el | 2745 | Workaround for gh#183 |
| sly.el | 2841 | Dead code suspected |
| sly.el | 3037 | Questionable design |
| sly.el | 3328-3329 | Messy code, wrong location for check |
| sly.el | 3980 | Group symbols in apropos |
| sly.el | 4238 | Could use rename-buffer |
| sly.el | 4554 | Unfinished TODO |
| sly.el | 4744 | Button options |
| sly.el | 4991-4993 | Recompilation cruft, going to contrib |
| sly.el | 5425 | Could avoid eval in Emacs 25+ |
| sly.el | 6710 | Why narrow buffer? |
| sly.el | 6844 | Restore old version |
| sly.el | 6964 | Tricky Slynk calls in contrib enable |
| sly.el | 7127 | Similar to sly-alistify |
| sly.el | 7282-7283 | Let it crash, bogus constraint |
| lib/sly-tests.el | 219 | Recursive-edit issue |
| lib/sly-tests.el | 232 | Unused function |
| lib/sly-tests.el | 1144 | sly-wait-condition returns immediately |
| lib/sly-tests.el | 1314 | Wait one second suboptimal |
| lib/sly-tests.el | 1395 | Additional useful test |
| lib/sly-completion.el | 375-382 | Emulates completion--in-region |
| lib/sly-parse.el | 72 | Should use syntax-ppss |
| contrib/sly-fontifying-fu.el | 33 | Remove sly-search-suppressed-forms |
| slynk/slynk-rpc.lisp | 159 | Tell Emacs about encoding problem |
| slynk/slynk.lisp | 496 | Fugly, need for naming |
| slynk/slynk.lisp | 511 | Should incorporate more |
| slynk/slynk.lisp | 721 | Overdesigned |
| slynk/slynk.lisp | 730 | Access global variable |
| slynk/slynk.lisp | 810 | Confusing docstring |
| slynk/slynk.lisp | 1418 | Make use SLYNK-MATCH |
| slynk/slynk.lisp | 1590 | Not thread safe |
| slynk/slynk.lisp | 1798 | Deal with #\| portably |
| slynk/slynk.lisp | 2279 | 45 lines for simple task |
| slynk/slynk.lisp | 2380 | Check FORM for setfability |
| slynk/slynk.lisp | 2556 | Proper method-combination |
| slynk/slynk.lisp | 2692 | (last (compute-restarts)) dubious |
| slynk/slynk.lisp | 2868 | Compile-file-for-emacs-hook obsoletes this |
| slynk/slynk.lisp | 3459 | SBCL silly reason |
| slynk/slynk.lisp | 3478 | HACK for ENSURE-ISTATE-METADATA |

---

*Document generated from SLY source code analysis. Last updated: 2025.*
