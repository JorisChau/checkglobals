#' @title Cast to data.frame
#' @description Cast an S3-object of class \code{"checkglobals"} to a data.frame.
#' @param x object inheriting from class \code{"checkglobals"}.
#' @param pattern an optional \link{regular expression}.  Only names
#' matching \code{pattern} are returned. \code{\link{glob2rx}} can be
#' used to convert wildcard patterns to regular expressions.
#' @param which a character vector, either \code{"global"} to print all unrecognized global variables,
#' \code{"import"} to print all detected imported functions and variables, or both (default).
#' @param row.names currently not used, included for compatibility with \code{\link{as.data.frame}} generic.
#' @param optional currently not used, included for compatibility with \code{\link{as.data.frame}} generic.
#' @param ... additional arguments to configure the output:
#' \itemize{
#' \item \code{all.names}, a logical value.  If \code{TRUE}, all object names are returned.
#' If \code{FALSE}, names which begin with a \samp{.} are omitted. Defaults to \code{TRUE}.
#' \item \code{sorted}, a logical value indicating if the function/variable names should be sorted alphabetically.
#' Defaults to \code{TRUE}.
#' }
#' @return a data.frame with three character columns:
#' \itemize{
#' \item \code{name}, the name of the global or imported function/variable.
#' \item \code{package}, the import package, only applies to imported functions/variables.
#' \item \code{type}, the type of the detected entity, either \code{"global"} or \code{"import"}.
#' }
#' @seealso \code{\link{as.data.frame}}
#' @examples
#' ## R-package from folder
#' chk <- checkglobals(
#'   pkg = system.file(
#'     "unit_tests", "pkg", "testpkg",
#'     package = "checkglobals"
#'   )
#' )
#' as.data.frame(chk)
#'
#' ## include only selected imports
#' as.data.frame(chk, pattern = "coef", which = "import")
#' @export
as.data.frame.checkglobals <- function(x, row.names = NULL, optional = FALSE, pattern, which = c("global", "import"), ...) {
  which <- match.arg(which, c("global", "import"), several.ok = TRUE)
  dots <- list(...)
  all.names <- dots$all.names %||% TRUE
  sorted <- dots$sorted %||% TRUE
  dfr <- data.frame(name = character(0), package = character(0), type = character(0), stringsAsFactors = FALSE)
  if(is.element("global", which)) {
    globals <- objects(x$globals$env, pattern = pattern, all.names = all.names, sorted = sorted)
    if(length(globals)) {
      dfr <- data.frame(name = globals, package = NA_character_, type = "global", stringsAsFactors = FALSE)
    }
  }
  if(is.element("import", which)) {
    if(!missing(pattern)) {
      imports <- objects(x$imports$env, pattern = pattern, all.names = all.names, sorted = sorted)
      if(length(imports)) {
        pkgs <- mget(imports, envir = x$imports$env, inherits = FALSE)
      } else {
        pkgs <- list()
      }
    } else {
      pkgs <- as.list(x$imports$env, all.names = all.names, sorted = sorted)
    }
    if(length(pkgs)) {
      imports <- data.frame(
        name = rep.int(names(pkgs), times = lengths(pkgs)),
        package = unname(unlist(pkgs, recursive = FALSE)),
        type = "import",
        stringsAsFactors = FALSE
      )
      if(sorted) {
        dfr <- rbind(dfr, imports[order(imports$package, imports$name), ], make.row.names = FALSE)
      } else {
        dfr <- rbind(dfr, imports, make.row.names = FALSE)
      }
    }
  }
  dfr
}

#' @title Cast to data.frame
#' @description Cast an S3-object of class \code{"checkglobalsg"} to a data.frame.
#' @param x object inheriting from class \code{"checkglobalsg"}.
#' @inheritParams as.data.frame.checkglobals
#' @return a data.frame similar in format to \code{\link{as.data.frame.checkglobals}}.
#' @seealso \code{\link{as.data.frame.checkglobals}}
#' @examples
#' ## R-package from folder
#' chk <- checkglobals(
#'   pkg = system.file(
#'     "unit_tests", "pkg", "testpkg",
#'     package = "checkglobals"
#'   )
#' )
#' as.data.frame(chk$globals)
#' @export
as.data.frame.checkglobalsg <- function(x, row.names = NULL, optional = FALSE, pattern, ...) {
  as.data.frame.checkglobals(x = list(globals = x), pattern = pattern, which = "global", ...)
}

#' @title Cast to data.frame
#' @description Cast an S3-object of class \code{"checkglobalsi"} to a data.frame.
#' @param x object inheriting from class \code{"checkglobalsi"}.
#' @inheritParams as.data.frame.checkglobals
#' @return a data.frame similar in format to \code{\link{as.data.frame.checkglobals}}.
#' @seealso \code{\link{as.data.frame.checkglobals}}
#' @examples
#' ## R-package from folder
#' chk <- checkglobals(
#'   pkg = system.file(
#'     "unit_tests", "pkg", "testpkg",
#'     package = "checkglobals"
#'   )
#' )
#' as.data.frame(chk$imports)
#' @export
as.data.frame.checkglobalsi <- function(x, row.names = NULL, optional = FALSE, pattern, ...) {
  as.data.frame.checkglobals(x = list(imports = x), pattern = pattern, which = "import", ...)
}

#' @title Cast to matrix
#' @description Cast an S3-object of class \code{"checkglobals"} to a matrix.
#' @param x object inheriting from class \code{"checkglobals"}.
#' @inheritParams as.data.frame.checkglobals
#' @return a character matrix with three columns:
#' \itemize{
#' \item \code{name}, the name of the global or imported function/variable.
#' \item \code{package}, the import package, only applies to imported functions/variables.
#' \item \code{type}, the type of the detected entity, either \code{"global"} or \code{"import"}.
#' }
#' @seealso \code{\link{as.matrix}}
#' @examples
#' ## R-package from folder
#' chk <- checkglobals(
#'   pkg = system.file(
#'     "unit_tests", "pkg", "testpkg",
#'     package = "checkglobals"
#'   )
#' )
#' as.matrix(chk)
#'
#' ## include only selected imports
#' as.matrix(chk, pattern = "coef", which = "import")
#' @export
as.matrix.checkglobals <- function(x, pattern, which = c("global", "import"), ...) {
  dfr <- as.data.frame.checkglobals(x, pattern = pattern, which = which, ...)
  as.matrix(dfr)
}

#' @title Cast to matrix
#' @description Cast an S3-object of class \code{"checkglobalsg"} to a matrix.
#' @param x object inheriting from class \code{"checkglobalsg"}.
#' @inheritParams as.matrix.checkglobals
#' @return a matrix similar in format to \code{\link{as.matrix.checkglobals}}.
#' @seealso \code{\link{as.matrix.checkglobals}}
#' @examples
#' ## R-package from folder
#' chk <- checkglobals(
#'   pkg = system.file(
#'     "unit_tests", "pkg", "testpkg",
#'     package = "checkglobals"
#'   )
#' )
#' as.matrix(chk$globals)
#' @export
as.matrix.checkglobalsg <- function(x, pattern, ...) {
  as.matrix.checkglobals(x = list(globals = x), pattern = pattern, which = "global", ...)
}

#' @title Cast to matrix
#' @description Cast an S3-object of class \code{"checkglobalsi"} to a matrix.
#' @param x object inheriting from class \code{"checkglobalsi"}.
#' @inheritParams as.matrix.checkglobals
#' @return a matrix similar in format to \code{\link{as.matrix.checkglobals}}.
#' @seealso \code{\link{as.matrix.checkglobals}}
#' @examples
#' ## R-package from folder
#' chk <- checkglobals(
#'   pkg = system.file(
#'     "unit_tests", "pkg", "testpkg",
#'     package = "checkglobals"
#'   )
#' )
#' as.matrix(chk$imports)
#' @export
as.matrix.checkglobalsi <- function(x, pattern, ...) {
  as.matrix.checkglobals(x = list(imports = x), pattern = pattern, which = "import", ...)
}

#' @title Cast to character
#' @description Cast an S3-object of class \code{"checkglobals"} to a character vector.
#' @param x object inheriting from class \code{"checkglobals"}.
#' @inheritParams as.data.frame.checkglobals
#' @return a character vector containing the names of the global or imported function/variables.
#' @seealso \code{\link{as.character}}
#' @examples
#' ## R-package from folder
#' chk <- checkglobals(
#'   pkg = system.file(
#'     "unit_tests", "pkg", "testpkg",
#'     package = "checkglobals"
#'   )
#' )
#' as.character(chk)
#'
#' ## include only imports
#' as.character(chk, which = "import")
#' @export
as.character.checkglobals <- function(x, pattern, which = c("global", "import"), ...) {
  vec <- as_vector.checkglobals(x, pattern = pattern, which = which, ...)
  unname(unlist(vec[which]))
}

#' @title Cast to character
#' @description Cast an S3-object of class \code{"checkglobalsg"} to a character vector.
#' @param x object inheriting from class \code{"checkglobalsg"}.
#' @inheritParams as.character.checkglobals
#' @return a character vector similar to \code{\link{as.character.checkglobals}}.
#' @seealso \code{\link{as.character.checkglobals}}
#' @examples
#' ## R-package from folder
#' chk <- checkglobals(
#'   pkg = system.file(
#'     "unit_tests", "pkg", "testpkg",
#'     package = "checkglobals"
#'   )
#' )
#' as.character(chk$globals)
#' @export
as.character.checkglobalsg <- function(x, pattern, ...) {
  as.character.checkglobals(x = list(globals = x), pattern = pattern, which = "global", ...)
}

#' @title Cast to character
#' @description Cast an S3-object of class \code{"checkglobalsi"} to a character vector.
#' @param x object inheriting from class \code{"checkglobalsi"}.
#' @inheritParams as.character.checkglobals
#' @return a character vector similar to \code{\link{as.character.checkglobals}}.
#' @seealso \code{\link{as.character.checkglobals}}
#' @examples
#' ## R-package from folder
#' chk <- checkglobals(
#'   pkg = system.file(
#'     "unit_tests", "pkg", "testpkg",
#'     package = "checkglobals"
#'   )
#' )
#' as.character(chk$imports)
#' @export
as.character.checkglobalsi <- function(x, pattern, ...) {
  as.character.checkglobals(x = list(imports = x), pattern = pattern, which = "import", ...)
}


#' Cast to list vector generic
#' @description \code{as_vector} is a generic function to cast objects returned by \code{\link{checkglobals}},
#' \code{\link{check_pkg}} or \code{\link{check_source}} to list vectors. The function invokes particular
#' \emph{methods} which depend on the \code{\link{class}} of the first argument.
#' @param x an S3-object to convert.
#' @param pattern an optional \link{regular expression}.  Only names
#' matching \code{pattern} are returned. \code{\link{glob2rx}} can be
#' used to convert wildcard patterns to regular expressions.
#' @param which a character vector, either \code{"global"} to print all unrecognized global variables,
#' \code{"import"} to print all detected imported functions and variables, or both (default).
#' @param ... additional arguments to configure the returned output.
#' @return a list of character vectors.
#' @export
as_vector <- function(x, pattern, which, ...) {
  UseMethod("as_vector")
}

#' @title Cast to list vector
#' @description Cast an S3-object of class \code{"checkglobals"} to a list vector.
#' @inheritParams as.data.frame.checkglobals
#' @return a list consisting of three character vectors:
#' \itemize{
#' \item \code{global}, vector of global function/variable names.
#' \item \code{import}, vector of import function/variable names.
#' \item \code{package}, vector of import package names.
#' }
#' @examples
#' ## R-package from folder
#' chk <- checkglobals(
#'   pkg = system.file(
#'     "unit_tests", "pkg", "testpkg",
#'     package = "checkglobals"
#'   )
#' )
#' as_vector(chk)
#'
#' ## include only selected imports
#' as_vector(chk, pattern = "coef", which = "import")
#' @export
as_vector.checkglobals <- function(x, pattern, which = c("global", "import"), ...) {
  which <- match.arg(which, c("global", "import"), several.ok = TRUE)
  dots <- list(...)
  all.names <- dots$all.names %||% TRUE
  sorted <- dots$sorted %||% TRUE
  vec <- list()
  if(is.element("global", which)) {
    vec$global <- objects(x$globals$env, pattern = pattern, all.names = all.names, sorted = sorted)
  }
  if(is.element("import", which)) {
    ## as.list.environment has no pattern argument
    if(!missing(pattern)) {
      vec$import <- objects(x$imports$env, pattern = pattern, all.names = all.names, sorted = sorted)
      vec$package <- character(0)
      if(length(vec$import)) {
        pkgs <- unlist(mget(vec$import, envir = x$imports$env, inherits = FALSE), recursive = FALSE)
        vec$package <- unique(pkgs)
        if(sorted) {
          vec$package <- sort(vec$package)
        }
      }
    } else {
      imports <- as.list(x$imports$env, all.names = all.names, sorted = sorted)
      vec$import <- names(imports)
      vec$package <- unique(unlist(imports, recursive = FALSE))
      if(sorted) {
        vec$package <- sort(vec$package)
      }
    }
  }
  vec
}

#' @title Cast to list vector
#' @description Cast an S3-object of class \code{"checkglobalsg"} to a list vector.
#' @param x object inheriting from class \code{"checkglobalsg"}.
#' @inheritParams as_vector.checkglobals
#' @return a list consisting of one character vector:
#' \itemize{
#' \item \code{global}, vector of global function/variable names.
#' }
#' @seealso \code{\link{as_vector.checkglobals}}
#' @examples
#' ## R-package from folder
#' chk <- checkglobals(
#'   pkg = system.file(
#'     "unit_tests", "pkg", "testpkg",
#'     package = "checkglobals"
#'   )
#' )
#' as_vector(chk$globals)
#' @export
as_vector.checkglobalsg <- function(x, pattern, ...) {
  as_vector.checkglobals(x = list(globals = x), pattern, which = "global", ...)
}

#' @title Cast to list vector
#' @description Cast an S3-object of class \code{"checkglobalsi"} to a list vector.
#' @param x object inheriting from class \code{"checkglobalsi"}.
#' @inheritParams as_vector.checkglobals
#' @return a list consisting of two character vectors:
#' \itemize{
#' \item \code{import}, vector of import function/variable names.
#' \item \code{package}, vector of import package names.
#' }
#' @seealso \code{\link{as_vector.checkglobals}}
#' @examples
#' ## R-package from folder
#' chk <- checkglobals(
#'   pkg = system.file(
#'     "unit_tests", "pkg", "testpkg",
#'     package = "checkglobals"
#'   )
#' )
#' as_vector(chk$imports)
#' @export
as_vector.checkglobalsi <- function(x, pattern, ...) {
  as_vector.checkglobals(x = list(imports = x), pattern, which = "import", ...)
}

