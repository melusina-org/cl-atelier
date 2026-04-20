# Diagnosis 1: `run-all-tests` fails on the second consecutive call

**Phase:** 1
**Slice:** product/slice/011-linter-api-cleanup
**Recorded:** 2026-04-20
**Author:** Debugger
**Status:** Diagnosis only — no code changed.

[Debugger] Diagnosis — `(atelier/test:run-all-tests)` second consecutive call
================================================================================

The bug is intra-image test pollution: a slow testcase loads an
ASDF system from a temporary directory, the `with-temporary-directory`
unwind deletes the directory, but the ASDF system registry still holds
the stale `system-source-file`. A new slice-011 fast testcase walks
`asdf:map-systems` and dereferences every registered system's
source-file path via `truename`, which crashes on the dangling
`net.cl-user.acme.example.asd`. The first run survives because the
polluting testcase runs after the consumer; on the second run the
consumer runs first and trips on the leftover state.

## Symptom

Verbatim console output, second invocation:

```
Failed to find the TRUENAME of /private/var/folders/k8/9lbs8q_x1vv490yd3xxlrz3r0000gn/T/atelier-test-332568/net.cl-user.acme.example.asd:

         No such file or directory
```

Condition class: `SB-INT:SIMPLE-FILE-ERROR`.

## Reproduction

Smallest reliable trigger (single SBCL image, no subprocess):

```lisp
(ql:quickload "org.melusina.atelier/test")
(atelier/test:run-all-tests)   ; passes 495/495
(atelier/test:run-all-tests)   ; signals SIMPLE-FILE-ERROR
```

The first call always passes; the second call always fails.

## Evidence gathered

1. **Reproduced verbatim** in a fresh `sbcl --non-interactive` subprocess
   driving the two calls back-to-back: first run reports
   `Success: 495/495 (100%)`; second run signals
   `SIMPLE-FILE-ERROR` on
   `/private/var/folders/.../atelier-test-332568/net.cl-user.acme.example.asd`.

2. **State inspection between the runs.** After the first
   `run-all-tests` returns, the form
   `(asdf:registered-system "net.cl-user.acme.example")` returns a
   live `#<SYSTEM …>` whose
   `(asdf:system-source-file …)` is
   `#P"/private/var/folders/.../atelier-test-332568/net.cl-user.acme.example.asd"`.
   The temporary directory has been deleted by the `unwind-protect`
   in `call-with-temporary-directory`. The ASDF registry retains the
   pointer.

3. **Backtrace of the second-run failure** (top-relevant frames,
   verbatim from `sb-debug:print-backtrace`):

   ```
    8: (SB-IMPL::QUERY-FILE-SYSTEM #P".../atelier-test-332568/net.cl-user.acme.example.asd" :TRUENAME T)
    9: (TRUENAME #P".../atelier-test-332568/net.cl-user.acme.example.asd")
   10: ((LAMBDA (ATELIER::SYS) :IN ATELIER:COLLECT-SIBLING-SYSTEMS) #<ASDF/SYSTEM:SYSTEM "net.cl-user.acme.example">)
   11: (ASDF/SYSTEM-REGISTRY:MAP-SYSTEMS #<FUNCTION (LAMBDA (ATELIER::SYS) :IN ATELIER:COLLECT-SIBLING-SYSTEMS) {…}>)
   12: (ATELIER:COLLECT-SIBLING-SYSTEMS #<ASDF/SYSTEM:SYSTEM "org.melusina.atelier">)
   13: (ATELIER:COLLECT-ALL-SOURCE-FILES #<ASDF/SYSTEM:SYSTEM "org.melusina.atelier">)
   14: (ATELIER:COLLECT-LINT-FILES "org.melusina.atelier" :SCOPE :PROJECT)
   15: ((FLET CONFIDENCE::TESTCASE-BODY :IN ATELIER/TEST:VALIDATE-COLLECT-LINT-FILES-PROJECT-SCOPE))
   ```

4. **Source of pollution identified.** `test/template.lisp:64-75`
   contains `ensure-a-lisp-project-is-created-for-the-golden-path`,
   which calls `asdf:load-system` and `asdf:operate 'asdf:test-op`
   on `net.cl-user.acme.example/test` against an `.asd` file inside
   the temp directory created by `with-temporary-directory`. The body
   defensively calls `asdf:clear-system` for the three system names
   *before* loading (lines 68-70) but never calls it on the
   `unwind-protect` cleanup path. The temp directory is deleted; the
   ASDF entries remain.

5. **First-run vs second-run order is the trip-wire.** In
   `test/entry-point.lisp` (lines 77-80), `run-all-tests` runs
   `run-fast-tests` *before* `run-slow-tests`. The polluting
   testcase `testsuite-template` lives inside `run-slow-tests`
   (line 64). The new slice-011 testcase
   `validate-collect-lint-files-project-scope` lives inside
   `run-fast-tests` (line 208 of `test/lint.lisp`, called from
   `validate-lint-fast`). On the first run, the consumer runs
   before the polluter — no observable failure. On the second run,
   the consumer runs while the registry still carries the leftover
   entry from the first run's polluter.

6. **`collect-sibling-systems` walks every registered system,
   unconditionally.** `src/asdf.lisp:341-355`:

   ```lisp
   (defun collect-sibling-systems (system)
     ...
     (asdf:map-systems
      (lambda (sys)
        (let ((sys-file (asdf:system-source-file sys)))
          (when (and sys-file
                     (equal (truename sys-file) asd-truename))
            (push sys siblings))))))
   ```

   `truename` here is total: any registered system whose
   `system-source-file` no longer exists on disk causes the whole
   walk to crash. Slice 011 introduced this code path
   (per `implementation-1.md`, step 1) — before slice 011, no
   linter primitive walked `map-systems`, so the latent pollution
   stayed dormant.

## Hypotheses considered

1. **The temp directory deletion fails and leaves files behind.**
   Refuted: the `Failed to find the TRUENAME` message confirms the
   file is *missing*, which is the success outcome of
   `delete-directory-tree`. The temp directory is gone; the
   in-image ASDF entry is what persists.

2. **`asdf:clear-system` calls in the testcase fail and the
   pre-load defence is never effective.** Refuted: the first run
   passes 495/495, so the pre-load clear works as intended for the
   *first* invocation. The bug surfaces only on the second
   invocation, after a full first run has registered the system
   and then deleted its files.

3. **The new `validate-collect-lint-files-project-scope` test is
   itself buggy and creates the dangling system.** Refuted: this
   testcase only *reads* via `collect-lint-files`; it never loads
   or registers a system. The leftover entry was registered by
   `ensure-a-lisp-project-is-created-for-the-golden-path` in
   the previous run.

4. **A `*central-registry*` entry is the culprit.** Refuted: the
   testcase wraps its registry mutation in a `let` binding on
   `asdf:*central-registry*` (line 67), which restores the binding
   on exit. The leak is in `asdf/system-registry::*defined-systems*`,
   the global table that `asdf:load-system` writes to, which has no
   dynamic-binding scope.

## Root cause

`ensure-a-lisp-project-is-created-for-the-golden-path`
(`/Users/michael/Melusina/atelier/test/template.lisp:64-75`) loads
ASDF systems from a temporary directory that is deleted on unwind,
but never calls `asdf:clear-system` on those systems on the
unwind path. The ASDF system registry (`*defined-systems*`)
therefore retains a `system` whose `system-source-file` points at
a deleted file. Slice 011's new project-scope linter primitive
walks `asdf:map-systems` and calls `truename` on every entry,
which signals `simple-file-error` on the dangling entry.

The fault has two halves; together they produce the observed
intermittent-on-second-run failure:

- **Pollution source** (the bug's origin):
  `test/template.lisp:64-75` mutates global ASDF state without
  matching cleanup.
- **Pollution amplifier** (the bug's revealer):
  `src/asdf.lisp:341-355` (`collect-sibling-systems`) treats
  `truename` as total and crashes the whole walk on a single stale
  entry.

## Proposed fix

**Primary fix (at the source of mutation, minimal):** in
`/Users/michael/Melusina/atelier/test/template.lisp`, wrap the
load-and-test body of
`ensure-a-lisp-project-is-created-for-the-golden-path` in
`unwind-protect` so the three `asdf:clear-system` calls run on
both normal and abnormal exit. Concretely, restructure
lines 67-75 from:

```lisp
(let ((asdf:*central-registry* (cons pathname asdf:*central-registry*)))
  (asdf:clear-system "net.cl-user.acme.example")
  (asdf:clear-system "net.cl-user.acme.example/test")
  (asdf:clear-system "net.cl-user.acme.example/development")
  (asdf:load-system "net.cl-user.acme.example/test")
  (asdf:operate 'asdf:test-op "net.cl-user.acme.example")
  (uiop:symbol-call "EXAMPLE/TEST" "RUN-ALL-TESTS")
  (asdf:load-system "net.cl-user.acme.example/development")
  (uiop:symbol-call "EXAMPLE/DEVELOPMENT" "LINT"))
```

to:

```lisp
(let ((asdf:*central-registry* (cons pathname asdf:*central-registry*)))
  (asdf:clear-system "net.cl-user.acme.example")
  (asdf:clear-system "net.cl-user.acme.example/test")
  (asdf:clear-system "net.cl-user.acme.example/development")
  (unwind-protect
       (progn
         (asdf:load-system "net.cl-user.acme.example/test")
         (asdf:operate 'asdf:test-op "net.cl-user.acme.example")
         (uiop:symbol-call "EXAMPLE/TEST" "RUN-ALL-TESTS")
         (asdf:load-system "net.cl-user.acme.example/development")
         (uiop:symbol-call "EXAMPLE/DEVELOPMENT" "LINT"))
    (asdf:clear-system "net.cl-user.acme.example")
    (asdf:clear-system "net.cl-user.acme.example/test")
    (asdf:clear-system "net.cl-user.acme.example/development")))
```

This keeps the test's intent local and explicit. The same idiom
should be applied to any other testcase that calls
`asdf:load-system` against a system inside `with-temporary-directory`
(grep confirms only this one site does so today).

**Secondary fix (defense in depth at the consumer):** in
`/Users/michael/Melusina/atelier/src/asdf.lisp:341-355`, make
`collect-sibling-systems` skip ASDF entries whose source file no
longer exists, instead of crashing the entire walk. Replace the
unconditional `(equal (truename sys-file) asd-truename)` with a
guarded version that uses `probe-file` (which returns `NIL` for
missing files and never signals) before `truename`, e.g.:

```lisp
(when (and sys-file
           (probe-file sys-file)
           (equal (truename sys-file) asd-truename))
  (push sys siblings))
```

Apply the same guard inside `collect-all-source-files`
(`src/asdf.lisp:367-370`) for the `asd-file` truename call,
which has the same shape. This protects every future consumer of
`map-systems` from any other test or REPL session that leaves a
stale system around. The Maker should treat this as a separate
commit so the diagnostic chain stays clean.

**Regression test (recommended, minimal):** add a fast testcase
asserting that `(atelier/test:run-all-tests)` is idempotent across
two consecutive invocations in the same image — calling
`run-all-tests` twice in a row must succeed both times. This
encodes the invariant the slice 011 retrospective implicitly
assumed.

## Adjacent risks

- Other testcases that load ASDF systems from temp directories
  would carry the same defect; a `Grep` for `asdf:load-system`
  inside `test/` shows only `template.lisp` as a current site,
  but `with-temporary-directory` is used elsewhere — the Maker
  should grep for any pattern combining `with-temporary-directory`
  with `asdf:load-system`, `asdf:test-system`, or
  `asdf:operate '…`. A symmetric `with-temporary-asdf-system`
  helper that wraps `unwind-protect` around `clear-system` would
  be a nice consolidation, but is out of scope for the diagnosis.
- The same `truename`-on-stale-entry crash shape appears in any
  consumer that walks `asdf:map-systems` (third-party tooling,
  IDE introspection, future Atelier inspectors). The defense-in-
  depth fix at `collect-sibling-systems` only covers Atelier's
  own paths.
- `collect-all-source-files` (`src/asdf.lisp:357-378`) takes
  `(truename asd-file)` of the input system itself
  (line 368). If the *requested* system has been cleared but
  not unregistered, the same crash shape can manifest there
  with a different trigger. Same `probe-file` guard applies.

## Named failure-mode tag

**Test pollution / global-state leak across testcase invocations**
(`/aifsd` §6.1, "tests as design pressure" — the symmetric
counterpart to the documented "tests aren't run on a fresh image"
failure). The polluter is a slow integration test whose cleanup
covers files but not in-image ASDF state; the amplifier is a
pure consumer that assumes the registry is consistent. Slice 011
introduced the amplifier and made a previously dormant
test-pollution latent for years immediately observable.
