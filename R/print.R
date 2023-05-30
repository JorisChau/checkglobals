#' @title Print \code{"checkglobals"} object
#'
#' @description Print method for S3-objects of class \code{"checkglobals"} as returned by \code{\link{checkglobals}},
#' \code{\link{check_pkg}} or \code{\link{check_source}}. Prints the \emph{name} and \emph{location}
#' of all unrecognized global variables; and the \emph{name} and \emph{location} of all detected imported functions
#' grouped by R-package. The \emph{location} consists of the source file name and line number. If
#' \href{https://CRAN.R-project.org/package=cli}{cli} is installed and cli-hyperlinks are supported in the console,
#' clicking the \emph{location} links opens the source file at the given line number. The bars printed behind the
#' import package names are filled based on the absolute number of detected imports per package.
#'
#' @param x object inheriting from class \code{"checkglobals"}.
#' @param format character, one of the following two choices:
#' \itemize{
#' \item \code{"basic"}, (default) prints only the name and source code location of the detected globals and imports.
#' \item \code{"detail"}, prints the name and location of the detected globals and imports, as well as the
#' lines in the source code file comprising the detected globals and imports. The maximum number of lines
#' printed per source code reference can be specified using \code{maxLines}.
#' }
#' @param pattern an optional \link{regular expression}.  Only names
#' matching \code{pattern} are returned. \code{\link{glob2rx}} can be
#' used to convert wildcard patterns to regular expressions.
#' @param which a character vector, either \code{"global"} to print all unrecognized global variables,
#' \code{"import"} to print all detected imported functions and variables, or both (default).
#' @param ... additional arguments to configure the printed output.
#' The following arguments can be specified:
#' \itemize{
#' \item \code{all.names}, a logical value.  If \code{TRUE}, all object names are returned.
#' If \code{FALSE}, names which begin with a \samp{.} are omitted. Defaults to \code{TRUE}.
#' \item \code{maxRef}, the maximum number of printed source code references per detected global/import.
#' Defaults to 1.
#' \item \code{maxLines}, the maximum number of printed lines per source code reference, only used if
#' \code{format = "detail"}. Defaults to 5.
#' \item \code{use_cli}, a logical value indicating if \code{cli} should be used to format the printed output.
#' Defaults to \code{TRUE}, which means that \code{cli}-formatting is attempted if \code{cli} is installed.
#' \item \code{maxWidth}, the maximum column width of the printed output. If \code{cli} is installed, the default
#' width is determined by \code{cli::console_width()}. If \code{cli} is not installed, \code{getOption("width")}
#' is checked. If \code{getOption("width")} is undefined, the column width defaults to 80.
#' }
#' @return Returns the object \code{x} \emph{invisibly} (via \code{\link{invisible}}).
#' @seealso \code{\link{checkglobals}}, \code{\link{check_pkg}}, \code{\link{check_source}}
#' @examples
#' ## R-package from folder
#' chk <- checkglobals(
#'   pkg = system.file(
#'     "unit_tests", "pkg", "testpkg",
#'     package = "checkglobals"
#'   )
#' )
#' chk
#'
#' ## print globals with references to source code
#' print(chk, format = "detail", which = "global", maxRef = 99)
#'
#' ## print selected imports
#' print(chk, format = "detail", pattern = "coef", which = "import", maxRef = 99)
#'
#' ## print without cli-formatting
#' print(chk, use_cli = FALSE)
#'
#' @export
print.checkglobals <- function(x, format = c("basic", "detail"), pattern, which = c("global", "import"), ...) {
  format <- match.arg(format, c("basic", "detail"))
  which <- match.arg(which, c("global", "import"), several.ok = TRUE)
  dots <- list(...)
  maxLines <- dots$maxLines %||% 5L
  maxRef <- dots$maxRef %||% 1L
  all.names <- dots$all.names %||% TRUE
  use_cli <- dots$use_cli %||% requireNamespace("cli", quietly = TRUE)
  maxWidth <- dots$maxWidth %||% if(use_cli) cli::console_width() else getOption("width") %||% 80L
  stopifnot(
    is.numeric(maxLines) && !is.na(maxLines),
    is.numeric(maxRef) && !is.na(maxRef),
    is.logical(all.names) && !is.na(all.names),
    is.logical(use_cli) && !is.na(use_cli),
    is.numeric(maxWidth) && !is.na(maxWidth)
  )
  if(length(x$missing_pkgs)) {
    fmt_pkg_warning(x$missing_pkgs, use_cli)
  }
  globalnms <- character(0)
  importnms <- character(0)
  if(is.element("global", which)) {
    globalnms <- objects(x$globals$env, pattern = pattern, all.names = all.names, sorted = TRUE)
  }
  if(is.element("import", which)) {
    importnms <- objects(x$imports$env, pattern = pattern, all.names = all.names, sorted = TRUE)
  }
  if(length(globalnms) || length(importnms)) {
    if(is.element("global", which)) {
      fmt_h1("Unrecognized global functions or variables", maxWidth, use_cli)
      if(length(globalnms)) {
        globals <- unlist(mget(globalnms, envir = x$globals$env, inherits = FALSE), recursive = FALSE)
        srcref_globals <- x$globals$srcref[globalnms]
        fmt_globals(globals, srcref_globals, use_cli)
      } else {
        cat("\n")
        fmt_success("None detected", use_cli)
      }
    }
    if(is.element("import", which)) {
      fmt_h1("Detected imported functions or variables", maxWidth, use_cli)
      if(length(importnms)) {
        importslist <- mget(importnms, envir = x$imports$env, inherits = FALSE)
        imports <- unlist(importslist, recursive = FALSE)
        names(imports) <- rep(names(importslist), times = lengths(importslist))
        imports <- sort(imports)
        srcref_imports <- x$imports$srcref[names(imports)]
        fmt_imports(imports, srcref_imports, use_cli)
      } else {
        cat("\n")
        fmt_success("None detected", use_cli)
      }
    }
    if(is.element("detail", format)) {
      if(length(globalnms)) {
        fmt_h1("Global source code references", maxWidth, use_cli)
        cat("\n")
        for(nm in globalnms) {
          nRef <- length(x$globals$srcref[[nm]])
          for(j in seq_len(min(nRef, max(maxRef, 1)))) {
            fmt_detail(x$globals$srcref, nm, j, maxLines, maxWidth, use_cli)
          }
          if(nRef > maxRef) {
            msg <- sprintf("  %s at %d more location%s...\n", nm, nRef - maxRef, ifelse(nRef - maxRef > 1, "s", ""))
            if(use_cli) {
              msg <- cli::col_grey(cli::style_italic(msg))
            }
            cat(msg)
          }
        }
      }
      if(length(importnms)) {
        fmt_h1("Import source code references", maxWidth, use_cli)
        cat("\n")
        for(nm in importnms) {
          nRef <- length(x$imports$srcref[[nm]])
          for(j in seq_len(min(nRef, max(maxRef, 1)))) {
            fmt_detail(x$imports$srcref, nm, j, maxLines, maxWidth, use_cli)
          }
          if(nRef > maxRef) {
            msg <- sprintf("  %s at %d more location%s...\n", nm, nRef - maxRef, ifelse(nRef - maxRef > 1, "s", ""))
            if(use_cli) {
              msg <- cli::col_grey(cli::style_italic(msg))
            }
            cat(msg)
          }
        }
      }
    }
  } else {
    msg <- sprintf("No %s functions or variables found",
                   ifelse(all(c("global", "import") %in% which), "unrecognized global and imported",
                          ifelse("global" %in% which, "unrecognized global", "imported")))
    fmt_success(msg, use_cli)
  }
  invisible(x)
}

#' @title Print \code{"checkglobalsg"} object
#' @description Print method for S3-objects of class \code{"checkglobalsg"} characteristic to the \code{"globals"}
#' list element of \code{"checkglobals"} objects returned by \code{\link{checkglobals}},
#' \code{\link{check_pkg}} or \code{\link{check_source}}.
#' @param x object inheriting from class \code{"checkglobalsg"}.
#' @param format character, one of the following two choices:
#' \itemize{
#' \item \code{"basic"}, (default) prints only the name and source code location of the detected globals.
#' \item \code{"detail"}, prints the name and location of the detected globals, as well as the
#' lines in the source code file comprising the detected globals. The maximum number of lines
#' printed per source code reference can be specified using \code{maxLines}.
#' }
#' @inheritParams print.checkglobals
#' @return Returns the object \code{x} \emph{invisibly} (via \code{\link{invisible}})
#' @examples
#' ## R-package from folder
#' chk <- checkglobals(
#'   pkg = system.file(
#'     "unit_tests", "pkg", "testpkg",
#'     package = "checkglobals"
#'   )
#' )
#' chk$globals
#'
#' ## print globals with references to source code
#' print(chk$globals, format = "detail", maxRef = 99)
#'
#' ## print without cli-formatting
#' print(chk$globals, use_cli = FALSE)
#' @export
print.checkglobalsg <- function(x, format = "basic", pattern, ...) {
  print.checkglobals(x = list(globals = x), format, pattern, which = "global", ...)
}

#' @title Print \code{"checkglobalsi"} object
#' @description Print method for S3-objects of class \code{"checkglobalsi"} characteristic
#' to the \code{"imports"} list element of \code{"checkglobals"} objects returned by \code{\link{checkglobals}},
#' \code{\link{check_pkg}} or \code{\link{check_source}}.
#' @param x object inheriting from class \code{"checkglobalsi"}.
#' @param format character, one of the following two choices:
#' \itemize{
#' \item \code{"basic"}, (default) prints only the name and source code location of the detected imports.
#' \item \code{"detail"}, prints the name and location of the detected imports, as well as the
#' lines in the source code file comprising the detected imports. The maximum number of lines
#' printed per source code reference can be specified using \code{maxLines}.
#' }
#' @inheritParams print.checkglobals
#' @return Returns the object \code{x} \emph{invisibly} (via \code{\link{invisible}})
#' @examples
#' ## R-package from folder
#' chk <- checkglobals(
#'   pkg = system.file(
#'     "unit_tests", "pkg", "testpkg",
#'     package = "checkglobals"
#'   )
#' )
#' chk$imports
#'
#' ## print selected imports
#' print(chk, format = "detail", pattern = "coef", which = "import", maxRef = 99)
#'
#' ## print without cli-formatting
#' print(chk$imports, use_cli = FALSE)
#' @export
print.checkglobalsi <- function(x, format = "basic", pattern, ...) {
  print.checkglobals(x = list(imports = x), format, pattern, which = "import", ...)
}

fmt_globals <- function(globals, srcref, use_cli) {
  refs <- fmt_srcref(srcref, use_cli)
  if(use_cli) {
    fmt <- fmt_align(
      list(
        c(cli::col_grey(cli::style_italic("<name>")),
          cli::code_highlight(paste0(names(globals), ifelse(globals == "function", "()", "")))),
        c(cli::col_grey(cli::style_italic("<location>")), refs)
      ),
      use_cli = TRUE
    )
  } else {
    fmt <- fmt_align(
      list(
        c("<name>", paste0(names(globals), ifelse(globals == "function", "()", ""))),
        c("<location>", refs)
      ),
      use_cli = FALSE
    )
  }
  cat("", fmt, sep = "\n")
}

fmt_imports <- function(imports, srcref, use_cli) {
  funsplit <- split(names(imports), f = imports)
  funinfo <- fmt_align(list(names(imports), fmt_srcref(srcref, use_cli)), use_cli = use_cli)
  if(use_cli) {
    mw <- max(cli::ansi_nchar(names(funsplit), "width"))
    pkginfo <- fmt_align(list(cli::style_bold(names(funsplit)), fmt_count(funsplit, srcref, use_cli = use_cli)), mw + 2, use_cli)
  } else {
    mw <- max(nchar(names(funsplit)))
    pkginfo <- fmt_align(list(names(funsplit), fmt_count(funsplit, srcref, use_cli = use_cli)), mw + 2, use_cli)
  }
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
    cat("\n")
    for(root in names(funsplit)) {
      cat(cli::tree(nodes, root = root), sep = "\n  ")
    }
  } else {
    cat("\n")
    funsplit <- utils::relist(funinfo, funsplit)
    names(pkginfo) <- names(funsplit)
    for(pkg in names(funsplit)) {
      cat(pkginfo[[pkg]], paste0("   \u2022", funsplit[[pkg]]), sep = "\n")
    }
  }
}

fmt_detail <- function(srcrefs, nm, j, maxLines, maxWidth, use_cli) {
  srcref <- srcrefs[[nm]][[j]]
  srcfile <- attr(srcref, "srcfile")
  srclines <- seq(from = srcref[1], to = srcref[3], by = 1L)
  label <- paste0(
    if(j == 1) {
      if(use_cli) {
        paste(cli::symbol$bullet, cli::style_bold(nm))
      } else {
        paste("\u2022", nm)
      }
    } else {
      paste(" ", nm)
    }, " at ",
    if(use_cli) {
      cli::col_blue(
        cli::style_hyperlink(
          text = paste(basename(srcfile$filename), srcref[1], sep = "#"),
          url = paste("file:", srcfile$filename, sep = "//"),
          params = list(line = srcref[1], col = 1)
        )
      )
    } else {
      paste(basename(srcfile$filename), srcref[1], sep = "#")
    }, ":"
  )
  if(maxLines > 0) {
    if(use_cli) {
      srccode <- paste0("    ", cli::col_grey(cli::style_italic(paste0(srclines, ": "))), gsub("\t", " ", as.character(srcref)))
      srccode <- cli::ansi_strtrim(srccode[1:min(length(srccode), maxLines)], width = maxWidth)
    } else {
      srccode <- paste0("    ", paste0(srclines, ": "), gsub("\t", " ", as.character(srcref)))
      srccode <- strtrim(srccode[1:min(length(srccode), maxLines)], width = maxWidth - 3)
    }
  } else {
    srccode <- NULL
  }
  cat(label, srccode, sep = "\n")
}

fmt_srcref <- function(srcref, use_cli) {
  vapply(
    srcref,
    FUN = function(src) {
      srcfile <- attr(src[[1]], "srcfile")
      if(use_cli) {
        paste(
          cli::col_blue(
            cli::style_hyperlink(
              text = paste(basename(srcfile$filename), src[[1]][1], sep = "#"),
              url = paste("file:", srcfile$filename, sep = "//"),
              params = list(line = src[[1]][1], col = 1)
            )
          ),
          if(length(src) > 1) {
            cli::col_grey(cli::style_italic(sprintf("and %d more...", length(src) - 1)))
          }
        )
      } else {
        paste(
          paste(basename(srcfile$filename), src[[1]][1], sep = "#"),
          if(length(src) > 1) {
            sprintf("and %d more...", length(src) - 1)
          }
        )
      }
    },
    FUN.VALUE = character(1),
    USE.NAMES = FALSE
  )
}

fmt_count <- function(funs, srcref, n = 10, use_cli) {
  count <- vapply(funs, function(fns) sum(lengths(srcref[fns])), integer(1))
  total <- sum(count)
  vapply(
    count,
    FUN = function(i) {
      if(use_cli) {
        if(i <= 2) {
          ibar <- c(rep(cli::col_red(cli::symbol$square_small_filled), i), rep(cli::col_grey(cli::symbol$square_small), n - i))
        } else if(i < 6) {
          ibar <- c(rep(cli::col_yellow(cli::symbol$square_small_filled), i), rep(cli::col_grey(cli::symbol$square_small), n - i))
        } else {
          ibar <- c(rep(cli::col_green(cli::symbol$square_small_filled), min(i, n)), rep(cli::col_grey(cli::symbol$square_small), n - min(i, n)))
        }
        paste0("[", paste(ibar, collapse = ""), "] ", cli::col_grey(cli::style_italic(sprintf("%d/%d", i, total))))
      } else {
        paste0("[", paste(c(rep("\u25FC", min(i, n)), rep("\u25FB", n - min(i, n))), collapse = ""), "] ", sprintf("%d/%d", i, total))
      }
    },
    FUN.VALUE = character(1)
  )
}

fmt_align <- function(cols, mw = NA, use_cli) {
  if(use_cli) {
    aligned <- lapply(cols, function(col) cli::ansi_align(col, align = "left", width = ifelse(is.na(mw), max(cli::ansi_nchar(col, "width")), mw)))
    cli::ansi_trimws(do.call(paste, c("", aligned)), "right")
  } else {
    aligned <- lapply(cols, function(col) paste0(col, strrep(" ", ifelse(is.na(mw), max(nchar(col)), pmax(max(nchar(col)), mw)) - nchar(col))))
    trimws(do.call(paste, c("", aligned)), "right")
  }
}

fmt_h1 <- function(text, maxWidth, use_cli) {
  if(use_cli) {
    cli::cli_h1(text)
  } else {
    text1 <- paste("\u2500\u2500", text, "", sep = " ")
    cat("\n", text1, paste(rep("\u2500", maxWidth - nchar(text1)), collapse = ""), "\n", sep ="")
  }
}

fmt_pkg_warning <- function(pkgs, use_cli) {
  if(use_cli) {
    cli::cli_alert_warning("Packages required but not installed: {.pkg {pkgs}}")
  } else {
    cat(sprintf("! Packages required but not installed: %s\n", pkgs))
  }
}

fmt_success <- function(msg, use_cli) {
  if(use_cli) {
    cli::cli_alert_success(msg)
  } else {
    cat(paste("\u2714", msg))
  }
}
