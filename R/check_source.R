#' Check R-scripts, folders or R-code strings for globals and imports
#'
#' Approximately detect global and imported functions or variables from R-scripts, folders or
#' R-code strings by static code analysis. For inspection of individual R-scripts use the `file` argument,
#' for R-code strings use the `text` argument, and for folders containing R-scripts use the `dir` argument.
#' This function does not require executing the code under inspection.
#'
#' @param file character path to R-script to analyze, can be either a file on the local filesystem or a
#' remote file location (e.g. a server or the web).
#' @param text character R-code string to analyze.
#' @param dir character path to folder with R-scripts to analyze.
#' @param include_compiled logical value indicating if compiled functions called with \code{\link{.Call}},
#' \code{\link{.C}}, \code{\link{.External}}, \code{\link{.Fortran}} should be included as global variables.
#' @param skip_globals optional character vector of names to skip/exclude as (unrecognized) global variables.
#' @return
#' list S3-object of class \code{"checkglobals"} with three components:
#' \itemize{
#' \item \code{globals}, list of class \code{"checkglobalsg"}
#' \item \code{imports}, list of class \code{"checkglobalsi"}
#' \item \code{missing_packages}, character vector with missing packages
#' }
#' for programmatic use, cast the returned S3-object with \code{\link{as.data.frame}}, \code{\link{as.matrix}},
#' \code{\link{as.character}} or \code{\link[checkglobals]{as_vector}}.
#' @seealso \code{\link{checkglobals}}, \code{\link{check_pkg}}
#' @examples
#' ## local R-script
#' check_source(
#'   file = system.file(
#'     "unit_tests", "pkg", "testpkg", "R", "functions1.R",
#'     package = "checkglobals"
#'   )
#' )
#'
#' ## local R-folder
#' check_source(
#'   dir = system.file(
#'     "unit_tests", "pkg", "testpkg", "R",
#'     package = "checkglobals"
#'   )
#' )
#'
#' ## R-code string
#' check_source(text = "cc <- function(m) stats::coef(m)")
#'
#' ## R-script from remote location
#' \donttest{
#'   check_source(
#'     file = "https://raw.githubusercontent.com/rstudio/shiny-examples/main/004-mpg/app.R"
#'   )
#' }
#' @export
check_source <- function(file, text, dir, include_compiled = FALSE, skip_globals = NULL) {

  ## parse code
  if(!missing(file)) {
    if(!file.exists(file)) {
      tmpdir <- tempdir(check = TRUE)
      if(grepl("\\://", file)) {
        utils::download.file(url = file, destfile = file.path(tmpdir, basename(file)))
        file <- file.path(tmpdir, basename(file))
      }
    }
    stopifnot(
      "'file' not found, make sure that 'file' is the path to an existing R-script." =
        file.exists(file),
      "'file' must be an R-script, run `knitr::purl()` on .Rmd files to extract the R-code first." =
        !grepl("\\.(rmd|rmarkdown)$", file, ignore.case = TRUE),
      "'file' format not recognized, make sure that 'file' is an existing R-script." =
        grepl("\\.r$", file, ignore.case = TRUE)
    )
    expr <- parse(file = file, keep.source = TRUE)
  } else if(!missing(text) && !is.null(text)) {
    expr <- parse(text = text, keep.source = TRUE)
  } else if(!missing(dir) && dir.exists(dir)) {
    files <- list.files(dir, pattern = "\\.[rR]$", recursive = TRUE, full.names = TRUE)
    if(!length(files)) {
      stop(sprintf("no R-scripts present in directory %s", dir))
    }
    expr <- lapply(files, parse, keep.source = TRUE)
  } else {
    stop("One of 'file', 'text' or 'dir' must be provided")
  }

  ## check R source code
  check <- .check_internal(
    expr = expr,
    include_compiled = include_compiled,
    include_datasets = FALSE,
    skip_globals = skip_globals
  )

  ## collect imports
  pkgs <- unique(get(".__pkgs__", envir = check$imports, inherits = FALSE))
  rm(list = ".__pkgs__", envir = check$imports, inherits = FALSE)
  missing_pkgs <- pkgs[!.find_pkgs(pkgs)]
  pkgs <- setdiff(pkgs, missing_pkgs)
  if(length(pkgs)) {
    pkgfuns <-  lapply(pkgs, function(p) {
      ns <- try(getNamespace(p), silent = TRUE)
      if(!inherits(ns, "try-error")) {
        exports <- names(.getNamespaceInfo(ns, "exports"))
        lazydata <- names(.getNamespaceInfo(ns, "lazydata"))
        nms <- c(exports, lazydata)
        vars <- replicate(length(nms), p, simplify = FALSE)
        names(vars) <- nms
        return(vars)
      } else {
        return(NULL)
      }
    })
    nsenv <- list2env(unlist(pkgfuns, recursive = FALSE), hash = TRUE, parent = emptyenv())
  } else {
    nsenv <- new.env(hash = TRUE, parent = emptyenv())
  }
  funs <- objects(check$imports, all.names = TRUE, sorted = FALSE)
  if(length(funs)) {
    fun_pkgs <- unique(unlist(mget(funs, envir = check$imports), recursive = FALSE))
    missing_fun_pkgs <- fun_pkgs[!.find_pkgs(fun_pkgs)]
    missing_pkgs <- union(missing_pkgs, missing_fun_pkgs)
  }

  ## move imported globals to imports
  globs <- objects(check$globals, all.names = TRUE, sorted = FALSE)
  srcrefg <- check$srcrefg
  srcrefi <- check$srcrefi
  if(length(globs)) {
    imports <- mget(globs, envir = nsenv, ifnotfound = NA_character_)
    isimport <- vapply(imports, Negate(is.na), logical(1))
    if(any(isimport)) {
      for(nm in names(imports)[isimport]) {
        assign(nm, unique(c(check$imports[[nm]], imports[[nm]])), envir = check$imports)
        srcrefi[[nm]] <- srcrefg[[nm]]
        srcrefg[[nm]] <- NULL
      }
      rm(list = names(imports)[isimport], envir = check$globals, inherits = FALSE)
    }
  }

  return(
    structure(
      list(
        globals = structure(
          list(
            env = check$globals,
            srcref = srcrefg
          ), class = "checkglobalsg"
        ),
        imports = structure(
          list(
            env = check$imports,
            srcref = srcrefi
          ), class = "checkglobalsi"
        ),
        missing_pkgs = missing_pkgs
      ), class = "checkglobals"
    )
  )

}
