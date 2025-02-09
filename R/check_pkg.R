#' Check R-packages for globals and imports
#'
#' Approximately detect global and imported functions or variables from R-packages by
#' static code analysis. Conceptually, the function inspects all files in the package
#' R-folder and contrasts the detected (unrecognized) globals and imports against
#' the imports listed in the NAMESPACE of the R-package. R-scripts present elsewhere
#' in the package (i.e. not in the R-folder) are not scanned, as these are not
#' coupled to the package NAMESPACE file.
#'
#' @param pkg character path to R-package, can be either:
#' \itemize{
#' \item a local R-package folder;
#' \item path to bundled (tar.gz) R-package on local filesystem;
#' \item remote path to bundled (tar.gz) R-package, (e.g. a remote server or the web).
#' }
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
#' @seealso \code{\link{checkglobals}}, \code{\link{check_source}}
#' @useDynLib checkglobals, .registration = TRUE
#' @examples
#' ## from R-package folder
#' check_pkg(
#'   pkg = system.file(
#'     "unit_tests", "pkg", "testpkg",
#'     package = "checkglobals"
#'   )
#' )
#'
#' ## from bundled R-package
#' \donttest{
#'   check_pkg(
#'     pkg = "https://cran.r-project.org/src/contrib/tinytest_1.4.1.tar.gz"
#'   )
#' }
#' @export
check_pkg <- function(pkg = ".", include_compiled = FALSE, skip_globals = NULL) {

  ## create temporary pkg folder (if necessary)
  if(grepl("\\.tar\\.gz$", basename(pkg), ignore.case = TRUE)) {
    tmpdir <- tempdir(check = TRUE)
    pkgdir <- tempfile(pattern = "checkglobals", tmpdir = tmpdir)
    if(!file.exists(pkg)) {
      if(grepl("\\://", pkg)) {
        utils::download.file(url = pkg, destfile = file.path(tmpdir, basename(pkg)))
        pkg <- file.path(tmpdir, basename(pkg))
      } else {
        stop("'pkg' (tar.gz) is not available locally and is not a valid url.", call. = FALSE)
      }
    }
    if(isTRUE(dir.create(pkgdir))) {
      utils::untar(pkg, exdir = pkgdir)
      pkg <- list.dirs(pkgdir, full.names = TRUE, recursive = FALSE)[1]
    } else {
      stop(sprintf("failed to create temporary folder to unpack '%s'", basename(pkg)))
    }
  }
  if(!file.exists(file.path(pkg, "DESCRIPTION"))) {
    stop("'pkg' must be a package folder, check if 'pkg' contains a DESCRIPTION file.", call. = FALSE)
  }

  ## skip sysdata variables
  if(file.exists(file.path(pkg, "R", "sysdata.rda"))) {
    envsys <- new.env(parent = emptyenv())
    load(file.path(pkg, "R", "sysdata.rda"), envir = envsys)
    skip_globals <- c(skip_globals, objects(envsys, all.names = TRUE, sorted = FALSE))
  }

  ## check pkg R folder
  rfiles <- list.files(file.path(pkg, "R"), pattern = "\\.[rR]$", full.names = TRUE)
  expr <- lapply(rfiles, parse, keep.source = TRUE)
  check <- .check_internal(
    expr = expr,
    is_pkg = TRUE,
    include_compiled = include_compiled,
    skip_globals = skip_globals
  )

  ## collect pkg namespace imports
  ns <- parseNamespaceFile(basename(pkg), dirname(pkg))
  nsenv <- new.env(hash = TRUE, parent = emptyenv())
  missing_pkgs <- character(0)
  ns_pkgs <- character(0)
  if(length(ns$imports)) {
    is_pkg <- (lengths(ns$imports) == 1L)
    is_pkg[!is_pkg] <- vapply(ns$imports[!is_pkg], function(x) !is.null(names(x)), logical(1))
    pkgs <- ns$imports[is_pkg]
    found_pkgs <- .find_pkgs(pkgs)
    ns_pkgs <- c(ns_pkgs, names(found_pkgs))
    missing_pkgs <- names(found_pkgs)[!found_pkgs]
    pkgs <- pkgs[found_pkgs]
    if(length(pkgs)) {
      nspkg <- lapply(pkgs, function(p) {
        ns <- try(getNamespace(p[[1]]), silent = TRUE)
        if(!inherits(ns, "try-error")) {
          exports <- names(.getNamespaceInfo(ns, "exports"))
          lazydata <- names(.getNamespaceInfo(ns, "lazydata"))
          nms <- c(exports, lazydata)
          if("except" %in% names(p)) {
            nms <- setdiff(nms, p[["except"]])
          }
          vars <- replicate(length(nms), p[[1]], simplify = FALSE)
          names(vars) <- nms
          return(vars)
        } else {
          return(NULL)
        }
      })
      if(any(lengths(nspkg))) {
        nsenv <- list2env(unlist(nspkg, recursive = FALSE), hash = TRUE, parent = emptyenv())
      }
    }
    funs <- ns$imports[!is_pkg & lengths(ns$imports) == 2L]
    if(length(funs)) {
      if(max(lengths(nms <- lapply(funs, `[[`, 2L))) == 1L) {
        nsfun <- lapply(funs, `[[`, 1L)
        names(nsfun) <- nms
      } else {
        nsfun <- lapply(funs, function(f) {
          vars <- replicate(length(f[[2]]), f[[1]], simplify = FALSE)
          names(vars) <- f[[2]]
          return(vars)
        })
        nsfun <- unlist(nsfun, recursive = FALSE)
      }
      nsenv <- list2env(nsfun, hash = TRUE, parent = nsenv)
      fun_pkgs <- unique(unlist(nsfun, recursive = FALSE))
      missing_fun_pkgs <- fun_pkgs[!.find_pkgs(fun_pkgs)]
      missing_pkgs <- union(missing_pkgs, missing_fun_pkgs)
      ns_pkgs <- union(ns_pkgs, fun_pkgs)
    }
  }
  if(length(ns$importMethods)) {
    nsmethod <- lapply(ns$importMethods, function(m) {
      vars <- replicate(length(m[[2]]), m[[1]], simplify = FALSE)
      names(vars) <- m[[2]]
      return(vars)
    })
    nsmethod <- unlist(nsmethod, recursive = FALSE)
    nsenv <- list2env(nsmethod, hash = TRUE, parent = nsenv)
    method_pkgs <- unique(unlist(nsmethod, recursive = FALSE))
    missing_method_pkgs <- method_pkgs[!.find_pkgs(method_pkgs)]
    missing_pkgs <- union(missing_pkgs, missing_method_pkgs)
    ns_pkgs <- union(ns_pkgs, method_pkgs)
  }

  ## collect pkg R folder imports
  pkgs <- unique(get(".__pkgs__", envir = check$imports, inherits = FALSE))
  rm(list = ".__pkgs__", envir = check$imports, inherits = FALSE)
  missing_pkgs <- union(missing_pkgs, pkgs[!.find_pkgs(pkgs)])
  pkgs <- setdiff(pkgs, c(basename(pkg), missing_pkgs))
  if(length(pkgs)) {
    pkgfuns <-  lapply(unlist(pkgs), function(p) {
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
    if(any(lengths(pkgfuns))) {
      nsenv <- list2env(unlist(pkgfuns, recursive = FALSE), hash = TRUE, parent = nsenv)
    }
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
    imports <- mget(globs, envir = nsenv, ifnotfound = NA_character_, inherits = TRUE)
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
        missing_pkgs = missing_pkgs,
        loaded_pkgs = ns_pkgs
      ), class = "checkglobals"
    )
  )

}



