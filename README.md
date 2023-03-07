# Atelier – Atelier for Lisp developers

The atelier for Lisp developers is providing useful tools for Lisp
developpers such as project templates and a linter.

*This software is Copyright © 2017–2022 Michaël Le Barbier and
is distributed under the terms described in the LICENSE file.*

# Introduction

## Example how to create a new Lisp project

~~~ lisp
(org.melusina.atelier:new-lisp-project
	  #p"~/Lisp/atelier/"
	  :copyright-holder "Michaël Le Barbier"
	  :copyright-year "2023"
	  :project-filename "org.melusina.atelier"
	  :project-name "Atelier"
	  :project-description "An Atelier for Lisp developers"
	  :project-long-description
	  #.(concatenate 'string
        "The atelier for Lisp developers is providing useful tools "
	    "for Lisp developpers such as project templates and a linter.")
	  :homepage "https://github.com/melusina-org/cl-atelier"
	  :license :mit)
~~~
