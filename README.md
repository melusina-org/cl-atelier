# Atelier – Atelier for Lisp developers

The atelier for Lisp developers is providing useful tools for Lisp
developpers such as project templates and a linter.

[![Continuous Integration](https://github.com/melusina-org/cl-atelier/actions/workflows/continuous-integration.yaml/badge.svg)](https://github.com/melusina-org/cl-atelier/actions/workflows/continuous-integration.yaml)

*This software is Copyright © 2017–2023 Michaël Le Barbier and
is distributed under the terms described in the LICENSE file.*

# Introduction

## Example how to create a new Lisp project

~~~ lisp
(atelier:new-lisp-project
  #p"~/Lisp/example/"
  :copyright-holder "Michaël Le Barbier"
  :copyright-year "2023"
  :project-filename "net.cl-user.example"
  :project-name "Example"
  :project-description "An Example System for Atelier users"
  :project-long-description
  #.(concatenate 'string
    "The Example System for Lisp developers showcases "
    "the development and usage of the Atelier system.")
  :homepage "https://cl-user.net/acme/cl-example"
  :license :mit)
~~~

Once the project is defined it can be loaded and used as any normal
system.


## Run the tests with ASDF
~~~
(asdf:operate 'asdf:test-op (asdf:find-system "net.cl-user.example"))
~~~

## Run the tests from the REPL

~~~ lisp
(asdf:operate 'asdf:load-op (asdf:find-system "net.cl-user.example"))
(example/testsuite:run-all-tests)
~~~

## Run the linter

~~~ lisp
(example/development:lint)
~~~
