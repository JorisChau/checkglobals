#' Check R-source code or R-packages for globals and imports
#'
#' Approximately detect global and imported functions or variables from R-scripts, folders,
#' R-code strings or R-packages by static code analysis. This function is simply a convenience
#' wrapper around \code{\link{check_source}} and \code{\link{check_pkg}} and the return
#' value is the same as calling these functions directly. If called without a \code{file}, \code{dir},
#' \code{text} or \code{pkg} argument, the function is run in the current working directory.
#' If the current working directory is an R-package folder, this is identical to \code{checkglobals(pkg = ".")},
#' otherwise the behavior is the same as \code{checkglobals(dir = ".")}.
#'
#' @param ... can be any one of the following arguments:
#' \itemize{
#' \item \code{file}, file character path to R-script to analyze, can be either a file on the local filesystem or a
#' remote file location (e.g. a server or the web).
#' \item \code{text}, character R-code string to analyze.
#' \item \code{dir}, character path to folder with R-scripts to analyze.
#' \item \code{pkg}, character path to R-package, can be either:
#' \itemize{
#' \item a local R-package folder;
#' \item path to bundled (tar.gz) R-package on local filesystem;
#' \item remote path to bundled (tar.gz) R-package, (e.g. a remote server or the web).
#' }
#' }
#' @inheritParams check_pkg
#' @return
#' list S3-object of class \code{"checkglobals"} with three components:
#' \itemize{
#' \item \code{globals}, list of class \code{"checkglobalsg"}
#' \item \code{imports}, list of class \code{"checkglobalsi"}
#' \item \code{missing_packages}, character vector with missing packages
#' }
#' for programmatic use, cast the returned S3-object with \code{\link{as.data.frame}}, \code{\link{as.matrix}},
#' \code{\link{as.character}} or \code{\link[checkglobals]{as_vector}}.
#' @seealso \code{\link{check_source}}, \code{\link{check_pkg}}
#' @examples
#' ## local R-script
#' checkglobals(
#'   file = system.file(
#'     "unit_tests", "pkg", "testpkg", "R", "functions1.R",
#'     package = "checkglobals"
#'   )
#' )
#'
#' ## local R-folder
#' checkglobals(
#'   dir = system.file(
#'     "unit_tests", "pkg", "testpkg", "R",
#'     package = "checkglobals")
#' )
#'
#' ## R-code string
#' checkglobals(text = "cc <- function(m) stats::coef(m)")
#'
#' ## R-package from folder
#' checkglobals(
#'   pkg = system.file(
#'     "unit_tests", "pkg", "testpkg",
#'     package = "checkglobals"
#'   )
#' )
#'
#' ## R-script from remote location
#' \donttest{
#' checkglobals(
#'   file = "https://raw.githubusercontent.com/rstudio/shiny-examples/main/004-mpg/app.R"
#' )
#' }
#'
#' ## R-package from remote location
#' \donttest{
#' check_pkg(
#'   pkg = "https://cran.r-project.org/src/contrib/tinytest_1.4.1.tar.gz",
#'   skip_globals = "cluster"
#' )
#' }
#' @export
checkglobals <- function(..., include_compiled = FALSE, skip_globals = NULL) {
  if(...length()) {
    ## dispatch using function arguments
    arg <- match.arg(...names(), c("file", "text", "dir", "pkg"))
    if(is.element(arg, c("file", "text", "dir"))) {
      check <- check_source(..., include_compiled = include_compiled, skip_globals = skip_globals)
    } else {
      check <- check_pkg(..., include_compiled = include_compiled, skip_globals = skip_globals)
    }
  } else {
    ## dispatch using wd
    if(file.exists(file.path(".", "DESCRIPTION"))) {
      check <- check_pkg(pkg = ".", include_compiled = include_compiled, skip_globals = skip_globals)
    } else {
      check <- check_source(dir = ".", include_compiled = include_compiled, skip_globals = skip_globals)
    }
  }
  return(check)
}
