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

#' Cast to SARIF json
#' @description \code{as_sarif_json} is a generic function to cast objects returned by \code{\link{checkglobals}},
#' \code{\link{check_pkg}} or \code{\link{check_source}} to SARIF (Static Analysis Results Interchange Format) json
#' for upload to external CI tools (e.g. GitHub Code Scanning, Jenkins warnings-ng plugin or Azure DevOps).
#' The function invokes particular \emph{methods} which depend on the \code{\link{class}} of the first argument.
#' @param x an S3-object to convert.
#' @param path file path to write the SARIF json content to.
#' @param pattern an optional \link{regular expression}.  Only names
#' matching \code{pattern} are returned. \code{\link{glob2rx}} can be
#' used to convert wildcard patterns to regular expressions.
#' @param which a character vector, either \code{"global"} to include all unrecognized global variables,
#' \code{"import"} to include all detected imported functions and variables, or both (default).
#' @param ... additional arguments to configure the returned output.
#' @return json object.
#' @export
as_sarif_json <- function(x, path, pattern, which, ...) {
  UseMethod("as_sarif_json")
}

#' @title Cast to SARIF json
#' @description Cast an S3-object of class \code{"checkglobals"} to SARIF json.
#' @inheritParams as.data.frame.checkglobals
#' @param path (optional) file path to write the SARIF json content. If missing, the SARIF
#' content is returned as an object of class \code{json}.
#' @param ... additional arguments to configure the output:
#' \itemize{
#' \item \code{all.names}, a logical value. If \code{TRUE}, all object names are returned.
#' If \code{FALSE}, names which begin with a \samp{.} are omitted. Defaults to \code{TRUE}.
#' \item \code{pretty}, a logical value passed to \code{jsonlite::toJSON}. Defaults to \code{FALSE}.
#' \item \code{root_dir}, directory to use as root relative to which all result uri's are generated.
#' \item \code{use_cli}, a logical value indicating if \code{cli} should be used to format the result messages.
#' Defaults to \code{TRUE}, which means that \code{cli}-formatting is attempted if \code{cli} is installed.
#' \item \code{markdown}, a logical value indicating if \code{markdown} result fields should be included.
#'  Defaults to \code{FALSE}.
#' \item \code{first_only}, a logical value to include only the first detected location of an unknown
#' global variable or function (instead of all detected locations). Defaults to \code{FALSE}.
#' }
#' @return if \code{path} is provided writes the SARIF json content to \code{path} and
#' returns the \code{path} invisibly, otherwise returns the SARIF json content as a \code{json} object..
#' @examples
#' ## R-package from folder
#' chk <- checkglobals(
#'   pkg = system.file(
#'     "unit_tests", "pkg", "testpkg",
#'     package = "checkglobals"
#'   )
#' )
#' json <- as_sarif_json(chk, pretty = TRUE, markdown = TRUE)
#' @export
as_sarif_json.checkglobals <- function(x, path, pattern, which = c("global", "import"), ...) {
  stopifnot(
    "jsonlite must be installed to cast to a SARIF json file." =
      is.element("jsonlite", rownames(utils::installed.packages()))
  )
  which <- match.arg(which, c("global", "import"), several.ok = TRUE)
  use_cli <- requireNamespace("cli", quietly = TRUE)
  dots <- list(...)
  all.names <- dots$all.names %||% TRUE
  pretty <- dots$pretty %||% FALSE
  call_path <- normalizePath(attr(x, "call")[[2]], winslash = "/", mustWork = FALSE)
  uri_root <- dots$root_dir %||% call_path
  use_cli <- dots$use_cli %||% requireNamespace("cli", quietly = TRUE)
  include_markdown <- dots$markdown %||% FALSE
  first_only <- dots$first_only %||% FALSE

  if(!file.info(uri_root)$isdir) {
    path_files <- basename(uri_root)
    uri_root <- dirname(uri_root)
  } else {
    path_files <- rel_path(list.files(call_path, include.dirs = TRUE, full.names = TRUE), uri_root)
  }
  results <- list()

  ## missing packages
  if(length(x$missing_pkgs)) {
    uri <- resolve_uri_path(x, uri_root, path_files, "DESCRIPTION")
    if(identical(basename(uri), "DESCRIPTION")) {
      endLine <- tryCatch(length(readLines(file.path(uri_root, uri))), error = function(e) 1L)
    } else {
      endLine <- 1L
    }
    results_pkg <- lapply(x$missing_pkgs, result_missing_pkg_impl, uri = uri, endLine = endLine, include_markdown = include_markdown)
    results <- c(results, results_pkg)
  }

  ## globals
  if(is.element("global", which)) {
    globalnms <- objects(x$globals$env, all.names = all.names, sorted = TRUE)
    globals <- unlist(mget(globalnms, envir = x$globals$env, inherits = FALSE), recursive = FALSE)
    srcref_globals <- x$globals$srcref[globalnms]
    results_globals <- lapply(globalnms, function(nm) {
      result_global_impl(globals[nm], srcref_globals[[nm]], uri_root, include_markdown, first_only)
    })
    results <- c(results, results_globals)
  }

  ## imports
  if(is.element("import", which)) {
    importnms <- objects(x$imports$env, all.names = all.names, sorted = TRUE)
    if(missing(pattern)) {
      loaded_pkgs <- structure(x$loaded_pkgs, .Names = rep("n/a", length(x$loaded_pkgs)))
    }
    if(length(importnms) || length(loaded_pkgs)) {
      if(length(importnms)) {
        importslist <- mget(importnms, envir = x$imports$env, inherits = FALSE)
        imports <- unlist(importslist, recursive = FALSE)
        if(length(loaded_pkgs))
          loaded_pkgs <- setdiff(loaded_pkgs, unique(imports))
        names(imports) <- rep(names(importslist), times = lengths(importslist))
      } else {
        imports <- importslist <- NULL
      }
      if(length(imports)) {
        if(!exists("uri", mode = "character", inherits = FALSE)) {
          uri <- resolve_uri_path(x, uri_root, path_files, "DESCRIPTION")
          if(identical(basename(uri), "DESCRIPTION")) {
            endLine <- tryCatch(length(readLines(file.path(uri_root, uri))), error = function(e) 1L)
          } else {
            endLine <- 1L
          }
        }
        imports <- sort(imports)
        srcref_imports <- x$imports$srcref[names(imports)]
        result_imports <- result_imports_impl(imports, srcref_imports, uri, endLine, endLine, use_cli, include_markdown)
        results <- c(results, list(result_imports))
      }
      if(length(loaded_pkgs)) {
        if(!exists("uri", mode = "character", inherits = FALSE)) {
          uri <- resolve_uri_path(x, uri_root, path_files, "DESCRIPTION")
          if(identical(basename(uri), "DESCRIPTION")) {
            endLine <- tryCatch(length(readLines(file.path(uri_root, uri))), error = function(e) 1L)
          } else {
            endLine <- 1L
          }
        }
        results_unused <- lapply(loaded_pkgs, result_unused_pkg_impl, uri = uri, endLine = endLine, include_markdown = include_markdown)
        results <- c(results, results_unused)
      }
    }
  }

  ## add results to template
  lst <- jsonlite::read_json(path = system.file("extdata", "sarif.json", package = "checkglobals"), simplifyVector = FALSE)
  lst[["runs"]][[1]][["results"]] <- results

  if(missing(path)) {
    res <- jsonlite::toJSON(lst, pretty = pretty, auto_unbox = TRUE)
    return(res)
  } else {
    jsonlite::write_json(lst, path = path, pretty = pretty, auto_unbox = TRUE)
    return(invisible(path))
  }
}

result_missing_pkg_impl <- function(pkg, uri, startLine = 1L, endLine = startLine, include_markdown = FALSE) {
  msg <- sprintf("Package <code>%s</code> is required but not installed", pkg)
  return(
    list(
      ruleId = "CG02",
      level = "note",
      message = if(include_markdown) {
        list(
          markdown = msg,
          text = gsub("</?code>", "'", msg)
        )
      } else {
        list(
          text = gsub("</?code>", "'", msg)
        )
      },
      locations = list(
        list(
          physicalLocation = list(
            artifactLocation = list(
              uri = uri
            ),
            region = list(
              startLine = startLine,
              endLine = endLine

            )
          )
        )
      )
    )
  )
}

result_unused_pkg_impl <- function(pkg, uri, startLine = 1L, endLine = endLine, include_markdown = FALSE) {
  msg <-  sprintf("Package <code>%s</code> is loaded or imported but not used", pkg)
  return(
    list(
      ruleId = "CG03",
      level = "note",
      message = if(include_markdown) {
        list(
          markdown = msg,
          text = gsub("</?code>", "'", msg)
        )
      } else {
        list(
          text = gsub("</?code>", "'", msg)
        )
      },
      locations = list(
        list(
          physicalLocation = list(
            artifactLocation = list(
              uri = uri
            ),
            region = list(
              startLine = startLine,
              endLine = endLine
            )
          )
        )
      )
    )
  )
}

result_global_impl <- function(global, srcref, prj_root, include_markdown = FALSE, first_only = FALSE) {

  if(first_only && is.list(srcref) && length(srcref) > 1) {
    srcref1 <- srcref[1]
  } else {
    srcref1 <- srcref
  }
  locations <- lapply(srcref1, function(src) {
    if(inherits(src, "srcref")) {
      srcfile <- attr(src, "srcfile")
      list(
        physicalLocation = list(
          artifactLocation = list(
            uri = rel_path(srcfile$filename, prj_root)
          ),
          region = list(
            startLine = src[1],
            startColumn = ifelse(length(src) > 4, src[5], src[2]),
            endLine = src[3],
            endColumn = ifelse(length(src) > 4, src[6], src[4])
          )
        )
      )
    }
  })
  refs <- fmt_srcref(list(srcref), use_cli = FALSE)
  if(nzchar(refs)) {
    refs <- sprintf(" at %s", refs)
  }
  msg <- sprintf("Unrecognized global %s <code>%s%s</code>%s", global, names(global),
                 ifelse(global == "function", "()", ""), refs)
  return(
    list(
      ruleId = "CG01",
      level = "note",
      message = if(include_markdown) {
        list(
          markdown = msg,
          text = gsub("</?code>", "'", msg)
        )
      } else {
        list(
          text = gsub("</?code>", "'", msg)
        )
      },
      locations = locations
    )
  )
}

result_imports_impl <- function(imports, srcref, uri, startLine = 1L, endLine = startLine, use_cli = FALSE, include_markdown = FALSE) {

  funinfo <- trimws(fmt_srcref(srcref, FALSE), which = "right")
  funinfo <- fmt_align(list(names(imports), funinfo), use_cli = FALSE)
  funsplit <- split(names(imports), f = imports)
  mw <- max(nchar(names(funsplit)))
  pkgnms <- sprintf("%s", names(funsplit))
  pkginfo <- trimws(fmt_align(list(pkgnms, fmt_count(funsplit, srcref, use_cli = FALSE)), mw + 2, FALSE), which = "left")
  if(any(duplicated(c(names(funsplit), names(imports))))) {
    funnms <- make.unique(c(names(funsplit), names(imports)))
    funsplit <- utils::relist(funnms[-c(1:length(funsplit))], funsplit)
  } else {
    funnms <- c(names(funsplit), names(imports))
  }
  if(use_cli) {
    nodes <- data.frame(
      pkg = funnms,
      fun = I(c(funsplit, replicate(length(imports), character(0)))),
      info = c(pkginfo, funinfo),
      stringsAsFactors = FALSE
    )
    msgs <- lapply(names(funsplit), function(root) {
      trimws(paste(paste0("\u00A0\u00A0", cli::tree(nodes, root = root)), collapse = "\n"), whitespace = "\u00A0")
    })
    msgs <- do.call(paste, args = c(msgs, list(sep = "\n")))
  } else {
    funsplit <- utils::relist(funinfo, funsplit)
    names(pkginfo) <- names(funsplit)
    msgs <- lapply(names(funsplit), function(pkg) {
      paste(pkginfo[[pkg]], paste(paste0("\u00A0\u00A0\u2022", funsplit[[pkg]]), collapse = "\n\u00A0"), sep = "\n\u00A0")
    })
    msgs <- do.call(paste, args = c(msgs, list(sep = "\n")))
  }
  result <- list(
    ruleId = "CG04",
    level = "none",
    kind = "informational",
    message = if(include_markdown) {
      list(
        markdown = sprintf("<pre><code>%s</pre></code>", msgs),
        text = msgs
      )
    } else {
      list(
        text = msgs
      )
    },
    locations =  list(
      list(
        physicalLocation = list(
          artifactLocation = list(
            uri = uri
          ),
          region = list(
            startLine = startLine,
            endLine = endLine
          )
        )
      )
    )
  )
  return(result)
}

rel_path <- function(path, root) {
  path <- normalizePath(path, winslash = "/", mustWork = FALSE)
  root <- normalizePath(root, winslash = "/", mustWork = FALSE)
  rel <- basename(path)
  dir_path <- dirname(path[1])
  dir_path0 <- NA_character_
  while(!identical(dir_path, root) && !identical(dir_path, dir_path0)) {
    dir_path0 <- dir_path
    rel <- file.path(basename(dir_path), rel)
    dir_path <- dirname(dir_path)
  }
  if(!identical(dir_path, dir_path0)) {
    return(rel)
  } else {
    return(path)
  }
}

resolve_uri_path <- function(x, root, files, target) {
  ## not a directory
  if(identical(length(files), 1L) && !file.info(file.path(root, files))$isdir) {
    return(files)
  } else {
    file_nms <- basename(files)
    if(is.element(target, file_nms)) {
      return(files[match(target, file_nms)])
    } else {  ## placeholder
      srcfile <- attr(x$imports$srcref[[1]][[1]], "srcfile")
      file <- rel_path(srcfile$filename, root)
      return(file)
    }
  }
}
