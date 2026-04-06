# Inspection Pipeline Design

This document describes the `run-inspectors` pipeline — the sequence
of stages that inspects a single file. Each stage operates on the
output of the previous one. When automatic fixes are enabled, each
stage is a reduction: the inspector produces findings, fixable findings
are applied, and the result becomes the input to the next inspector.

---

## Pipeline Stages

```
FILE-0  (original file on disk)
  │
  ├── Stage 1: File inspectors
  │     for each file-inspector:
  │       findings ← (inspect-file inspector pathname)
  │       if auto-fix and fixable findings:
  │         FILE-i+1 ← apply fixes to FILE-i
  │     result: FILE-N, collected findings
  │
  ├── Stage 2: Line inspectors
  │     LINES-0 ← read FILE-N into lines (one read)
  │     for each line-inspector:
  │       findings ← (inspect-lines inspector lines)
  │       if auto-fix and fixable findings:
  │         LINES-i+1 ← apply fixes to LINES-i
  │     write LINES-N back as FILE-N+1
  │     result: FILE-N+1, collected findings
  │
  ├── Stage 3: Syntax inspectors (language-specific)
  │     CST-0 ← parse FILE-N+1 with Eclector
  │     for each syntax-inspector:
  │       findings ← (inspect-syntax inspector cst)
  │       if auto-fix and fixable findings:
  │         CST-i+1 ← apply transforms to CST-i
  │         (innermost nodes first, then top-level forms)
  │     pretty-print CST-N and write back as final file
  │     result: final file, collected findings
  │
  └── All findings returned
```

## Reduction Structure

Each stage has the same shape:

```
(reduce #'apply-inspector-and-collect-findings
        inspectors
        :initial-value initial-state)
```

Where the state carries both the current content (file bytes, line
vector, or CST) and the accumulated findings. Each inspector call
produces findings; fixable findings modify the content; the modified
content feeds the next inspector.

When auto-fix is disabled, the reduction degenerates into a map:
each inspector sees the original content, findings accumulate but
content does not change.

## Non-Orthogonal Fixes

If an automatic fix modifies content in a way that invalidates a
previous inspector's assumptions, the stage must re-run. Detection:
after all inspectors in a stage have run, re-run the stage on the
fixed content. If new findings appear, repeat. Terminate when a
fixpoint is reached or a maximum iteration count is exceeded.

This is deferred — the current implementation does not apply fixes.

## Restarts

A restart `write-back-and-stop` is installed around the pipeline.
Invoking it writes the current state (FILE-N, LINES-N, or CST-N)
back to disk and stops processing. This allows interactive users to
interrupt a long-running inspection and keep partial results.

This is deferred — the current implementation does not install restarts.

## Current Implementation (Slice 003)

Only stages 1 and 2 are implemented. No auto-fix, no write-back, no
fixpoint loop, no restarts. The runner:

1. Binds `*current-project-configuration*` and `*current-linter-configuration*`.
2. Runs all file-inspector instances on the pathname.
3. Reads the file into lines once.
4. Runs all line-inspector instances on the lines.
5. Returns the combined findings.

Stage 3 (syntax inspectors) is deferred to a later slice.
