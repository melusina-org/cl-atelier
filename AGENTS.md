# Atelier

## Overview

A Common Lisp companion for CI pipelines on SBCL. Implemented:
templating and linting with autofix. Planned: pretty-printing and
secret detection.

## Commands

* `development/testsuite`: run the full test suite.
* `development/testsuite validate-lint-lines`: run the single test
  case `validate-lint-lines`.
* `development/build [-v]`: cold build of `org.melusina.atelier` and
  its testsuite system. `-v` enables compiler verbosity.
* `development/lint`: identify anomalies in code.

## Conventions

* Function names are verb–noun: `make-resource`, `list-resources`,
  `find-resource`.
* Type predicates follow CL convention: `resource-p`.
* Other predicates use noun–adjective: `resource-ready-p`,
  `resource-created-p`.
* Reader and accessor names combine class and slot:
  `resource-identifier`, `resource-owner`.
* A slot with a reader or accessor is part of the package's public
  interface and is exported. Slot names themselves are not exported.

## Boundaries

* Never push to `main` — merges go through PR review.
* Never modify `LICENSE` — it is the legal record, not a source file.
* Run the full test suite and a cold build before committing — partial
  validation has masked breakage in the past.
* Each commit serves one purpose and ships with its tests — bisecting
  depends on it.
* Ask before modifying a test that broke from seemingly unrelated
  changes — the test may be catching a real regression.
* Ask before adding a dependency — every new dependency is a security and
  supply-chain commitment we audit by hand.
