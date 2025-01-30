utils::globalVariables("G")
utils::globalVariables(g)

"dataset"

.onLoad <- function(libname, pkgname) {
  fLoad <- function(x) x
  xauth <- utils::maintainer(pkgname)
}

assign(".onAttach", function(libname, pkgname) {
  fAttach <- function(x) {
    fAttach2(x)
  }
})
