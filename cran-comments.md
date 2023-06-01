# CRAN package version 0.1.0

## Author comments:

**The Title field should be in title case (NOTE)**
: changed title field to 'Static Analysis of R-Code Dependencies'.

**Which part of this package is not already provided by R check code and/or package codetools? (Question by U. Ligges)**
: the aim of this package is to identify undefined global, as well as, imported functions/variables, whereas R check and codetools (e.g. `findGlobals()`) are aimed at detecting (undefined) globals. In particular, `codetools::findGlobals()` historically fails to identify undefined names in various common use-cases, (e.g. character function names as arguments to functionals to name one scenario). R check works only for R-packages, and  `codetools::findGlobals()` (by itself) only operates on function objects. This package checks all of: individual R-scripts; folders containing R-scripts; R-code strings or R-packages, without strictly requiring e.g. installation of R-code dependencies. Compared to codetools, the code analysis is implemented in C for efficient syntax traversal of large code-bases.

**Please replace \dontrun with \donttest. (Comment by B. Altmann)**
: replaced`\dontrun{}` by `\donttest{}` for all examples that download data from the web.

## Test environments

* ubuntu gcc R-release, R-devel (rhub)
* debian gcc R-release, R-devel, R-patched (rhub)
* debian clang R-devel (rhub)
* fedora clang/gcc R-devel (rhub)
* macos-darwin20 clang R-release (gh-actions)
* windows gcc R-release, R-devel, R-oldrel, R-patched, R-4.1 (rhub, gh-actions)

## Compiled code checks

* ubuntu-rchk R-devel (rhub)
* ubuntu gcc R-4.2 --use-valgrind (local install)
* ubuntu gcc R-4.2 --use-gct (local install)
