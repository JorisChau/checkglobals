# CRAN package version 0.1.3

**Please replace \dontrun with \donttest. (Comment by B. Altmann)**
: replaced`\dontrun{}` by `\donttest{}` for all examples that download data from the web.

## Test environments

* ubuntu gcc R-oldrel, R-release, R-next, R-devel (rhub, gh-actions)
* debian gcc/clang R-devel (rhub)
* fedora gcc/clang R-devel (rhub)
* macos-14 clang R-release, R-next, R-devel (gh-actions, rhub)
* windows gcc R-oldrel, R-release, R-next, R-devel (r-winbuilder, gh-actions)

## Compiled code checks

* ubuntu-rchk R-devel (docker)
* fedora gcc R-devel --use-valgrind (rhub)
* ubuntu gcc R-4.4 --use-gct (local install)
