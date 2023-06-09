require(checkglobals)

cat("Running checkglobals unit tests...\n")

## check test
dotest <- function(itest, observed, expected) {
  if(!identical(observed, expected)) stop(sprintf("Test %s failed", itest), call. = FALSE)
}

## check R-file
check1 <- checkglobals(file = system.file("unit_tests", "pkg", "testpkg", "R", "functions1.R", package = "checkglobals"))

dotest("1.1", as.list(check1$globals$env), list(getMethod = "function", G = "variable", g = "variable",
                                                sysdata = "variable", pvec = "function", dataset = "variable"))
dotest("1.2", as.list(check1$imports$env),
       list(coef = c("stats", "stats4"), is.unit = "grid", getMethod = "methods", approxfun = "stats",
            R6Class = "R6", setGeneric = "methods", aggregate = "stats", head = "utils", setMethod = "methods",
            pvec = "parallel", median = "stats", tail = "utils"))
dotest("1.3", lengths(check1$globals$srcref), c(getMethod = 1L, G = 1L, g = 12L, sysdata = 1L, pvec =2L, dataset = 1L))
dotest("1.4", lengths(check1$imports$srcref),
       c(coef = 2L, getMethod = 1L, approxfun = 1L, R6Class = 1L, setGeneric = 1L, aggregate = 1L,
         head = 1L, setMethod = 1L, pvec = 1L, tail = 1L, is.unit = 1L, median = 2L))

## check verbose messages
rexpr <- parse(file = system.file("unit_tests", "pkg", "testpkg", "R", "functions1.R", package = "checkglobals"), keep.source = TRUE)
check2.1 <- capture.output(invisible(checkglobals:::.check_internal(rexpr, verbose = TRUE)))

dotest("2.1", check2.1, c("SYMBOL: f1", "SYMBOL_FORMALS: y", "SYMBOL: ff1", "SYMBOL_FORMALS: g",
                          "SYMBOL: f2", "SYMBOL_FORMALS: y", "SYMBOL: ff2", "SYMBOL_FORMALS: x",
                          "SYMBOL: f3", "SYMBOL_FORMALS: y", "SYMBOL: ff3", "SYMBOL: f4",
                          "SYMBOL_FORMALS: y", "ERROR: failed to evaluate call to requireNamespace",
                          "SYMBOL: pkg", "PKG_LOAD: utils", "SYMBOL: pkg", "PKG_LOAD: checkglobals",
                          "PKG_LOAD: checkglobals", "PKG_LOAD: stats", "SYMBOL: pkg", "PKG_LOAD: grid",
                          "SYMBOL: f6", "SYMBOL_FORMALS: y", "SYMBOL: x1", "SYMBOL: x2",
                          "SYMBOL: x3", "SYMBOL: x4", "SYMBOL: x5", "SYMBOL: x6", "SYMBOL: f7",
                          "SYMBOL_FORMALS: y", "SYMBOL: ff6", "PKG_SYMBOL: methods, setGeneric",
                          "SYMBOL: ff6", "PKG_SYMBOL: methods, setMethod", "SYMBOL_FORMALS: x",
                          "SYMBOL_FORMALS: x", "SYMBOL: f8", "SYMBOL_FORMALS: y", "SYMBOL: x",
                          "SYMBOL: f9", "SYMBOL_FORMALS: y", "PKG_SYMBOL: utils, head",
                          "PKG_SYMBOL: utils, head", "PKG_SYMBOL: utils, tail", "PKG_SYMBOL: methods, getMethod",
                          "PKG_SYMBOL: stats, approxfun", "PKG_SYMBOL: stats, coef", "PKG_SYMBOL: stats4, coef",
                          "PKG_SYMBOL: utils, head", "PKG_SYMBOL: utils, head", "SYMBOL: f10",
                          "SYMBOL_FORMALS: y", "SYMBOL: g", "SYMBOL: f11", "SYMBOL_FORMALS: y",
                          "PKG_SYMBOL: stats, aggregate", "Note: skipping globals in calls to '~'",
                          "PKG_SYMBOL: parallel, pvec", "SYMBOL: f12", "SYMBOL_FORMALS: y",
                          "COMPILED_FUN: Cfun", "SYMBOL: f13", "SYMBOL_FORMALS: y", "Note: skipping globals in calls to 'substitute'",
                          "Note: skipping globals in calls to 'quote'", "Note: skipping globals in calls to 'bquote'",
                          "Note: skipping globals in calls to 'Quote'", "SYMBOL: f14",
                          "SYMBOL_FORMALS: y", "SYMBOL: f15", "SYMBOL_FORMALS: ...", "SYMBOL: f16a",
                          "SYMBOL_FORMALS: y", "SYMBOL: set<-", "SYMBOL: f16b<-", "SYMBOL_FORMALS: var",
                          "SYMBOL_FORMALS: value", "SYMBOL: var", "SYMBOL: f17", "SYMBOL_FORMALS: y",
                          "SYMBOL: f18", "PKG_SYMBOL: R6, R6Class", "SYMBOL_FORMALS: ...", "SYMBOL_FORMALS: ...")
)

rexpr <- parse(file = system.file("unit_tests", "pkg", "testpkg", "R", "aaa.R", package = "checkglobals"), keep.source = TRUE)
check2.2 <- capture.output(invisible(checkglobals:::.check_internal(rexpr, include_datasets = TRUE, verbose = TRUE)))

dotest("2.2", check2.2, c("PKG_SYMBOL: utils, globalVariables", "ERROR: failed to evaluate call to globalVariables",
                          "PKG_SYMBOL: utils, globalVariables", "DATASET: dataset"))

## check string
check3 <- checkglobals(text = "g1 <- function(x, y) f1()")

## check directory
rdir <- system.file("unit_tests", "pkg", "testpkg", "R", package = "checkglobals")
check4.1 <- checkglobals(dir = rdir)
owd <- setwd(system.file("unit_tests", "pkg", "testpkg", "R", package = "checkglobals"))
check4.2 <- checkglobals()
setwd(owd)

## check R-package
check5.1 <- checkglobals(pkg = system.file("unit_tests", "pkg", "testpkg", package = "checkglobals"))
owd <- setwd(system.file("unit_tests", "pkg", "testpkg", package = "checkglobals"))
check5.2 <- checkglobals()
setwd(owd)

dotest("5.1", as.list(check5.1$globals$env), list(g = "variable"))
dotest("5.2", as.list(check5.1$imports$env),
       list(coef = c("stats", "stats4"), is.unit = "grid", getMethod = "methods", globalVariables = "utils",
            approxfun = "stats", R6Class = "R6", setGeneric = "methods", aggregate = "stats", head = "utils",
            setMethod = "methods", pvec = "parallel", median = "stats", tail = "utils"))
dotest("5.3", lengths(check5.1$globals$srcref), c(g = 13L))
dotest("5.4", lengths(check5.1$imports$srcref),
       c(coef = 2L, getMethod = 1L, globalVariables = 2L, approxfun = 1L, R6Class = 1L, setGeneric = 1L,
         aggregate = 1L, head = 1L, setMethod = 1L, pvec = 2L, tail = 1L, is.unit = 1L, median = 2L))

## check targz
tmpdir <- tempdir()
pkgdir <- tempfile(pattern = "testpkg", tmpdir = tmpdir)
if(isTRUE(dir.create(pkgdir)) && isTRUE(file.copy(dirname(rdir), pkgdir, recursive = TRUE)) && nzchar(Sys.which("R"))) {
  setwd(pkgdir); Sys.setenv("R_TESTS" = "");
  system(paste(Sys.which("R"), "CMD build testpkg --no-manual --no-build-vignettes --no-resave-data"))
  if(file.exists("testpkg_1.0.tar.gz")) {
    check6 <- checkglobals(pkg = "testpkg_1.0.tar.gz")
    check7 <- checkglobals(pkg = file.path("file:", normalizePath("testpkg_1.0.tar.gz"), fsep = "//"))
  }
  unlink(pkgdir, recursive = TRUE)
  setwd(owd)
}

## check methods
check8.1 <- as_vector(check5.1, pattern = "g")
check8.2 <- as_vector(check5.1$globals)
check8.3 <- as_vector(check5.1$imports, sorted = FALSE)  ## sorting depends on locale
check8.4 <- as.data.frame(check5.1$globals)
check8.5 <- as.data.frame(check5.1$imports, pattern = ".", sorted = FALSE)
check8.6 <- as.matrix(check5.1$globals)
check8.7 <- as.matrix(check5.1$imports, sorted = FALSE)
check8.8 <- as.character(check5.1$globals)
check8.9 <- as.character(check5.1$imports, sorted = FALSE)
check8.10 <- as.data.frame(check5.1$imports, sorted = FALSE)
check8.11 <- as.data.frame(check5.1$imports, pattern = "zzz")

dotest("8.1", check8.1, list(global = "g", import = c("aggregate", "getMethod", "globalVariables"), package = c("methods", "stats", "utils")))
dotest("8.2", check8.2, list(global = "g"))
dotest("8.3", check8.3, list(import = c("coef", "is.unit", "getMethod", "globalVariables",
                                        "approxfun", "R6Class", "setGeneric", "aggregate", "head", "setMethod",
                                        "pvec", "median", "tail"),
                             package = c("stats", "stats4", "grid", "methods", "utils", "R6", "parallel")))
dotest("8.4", check8.4, structure(list(name = "g", package = NA_character_, type = "global"), class = "data.frame", row.names = c(NA, -1L)))
dotest("8.5", check8.5, structure(list(name = c("coef", "coef", "is.unit", "getMethod",
                                                "globalVariables", "approxfun", "R6Class", "setGeneric", "aggregate",
                                                "head", "setMethod", "pvec", "median", "tail"),
                                       package = c("stats", "stats4", "grid", "methods", "utils", "stats", "R6", "methods",
                                                   "stats", "utils", "methods", "parallel", "stats", "utils"),
                                       type = c("import", "import", "import", "import", "import", "import", "import", "import",
                                                "import", "import", "import", "import", "import", "import")),
                                  row.names = c(NA, -14L), class = "data.frame"))
dotest("8.6", check8.6, structure(c("g", NA, "global"), dim = c(1L, 3L), dimnames = list(NULL, c("name", "package", "type"))))
dotest("8.7", check8.7, structure(c("coef", "coef", "is.unit", "getMethod", "globalVariables",
                                    "approxfun", "R6Class", "setGeneric", "aggregate", "head", "setMethod",
                                    "pvec", "median", "tail", "stats", "stats4", "grid", "methods",
                                    "utils", "stats", "R6", "methods", "stats", "utils", "methods",
                                    "parallel", "stats", "utils", "import", "import", "import", "import",
                                    "import", "import", "import", "import", "import", "import", "import",
                                    "import", "import", "import"), dim = c(14L, 3L), dimnames = list(NULL, c("name", "package", "type"))))
dotest("8.8", unname(check8.8), check8.2[["global"]])
dotest("8.9", unname(check8.9), check8.3[["import"]])
dotest("8.10", check8.10, check8.5)
dotest("8.11", check8.11, structure(list(name = character(0), package = character(0), type = character(0)), class = "data.frame", row.names = integer(0)))

## errors
tools::assertError(check_source())
tools::assertError(check_source(file = tempfile(tmpdir = tmpdir, pattern = "checkglobals", fileext = ".R")))
tools::assertError(check_source(file = tempfile(tmpdir = tmpdir, pattern = "checkglobals", fileext = ".Rmd")))
tools::assertError(check_source(file = tempfile(tmpdir = tmpdir, pattern = "checkglobals", fileext = ".Qmd")))
tools::assertError(check_pkg(pkg = tempfile(tmpdir = tmpdir, pattern = "checkglobals", fileext = ".tar.gz")))
tools::assertError(check_pkg(pkg = tmpdir))

## print to console
suppressMessages({
  invisible(
    capture.output({
      print(check5.1)
      print(check5.1$globals, pattern = "_")
      print(check5.1$globals, format = "detail", maxRef = 2)
      print(check5.1$imports, pattern = "globalVariables", format = "basic")
      print(check5.1$imports, pattern = "globalVariable", format = "detail", maxLines = 0)
    })
  )
})

suppressMessages({
  invisible(
    capture.output({
      print(check5.1, use_cli = FALSE)
      print(check5.1$globals, pattern = "_", use_cli = FALSE)
      print(check5.1$globals, format = "detail", maxRef = 2, use_cli = FALSE)
      print(check5.1$imports, pattern = "globalVariables", format = "basic", use_cli = FALSE)
      print(check5.1$imports, pattern = "globalVariable", format = "detail", maxLines = 0, use_cli = FALSE)
    })
  )
})

cat("Completed checkglobals unit tests\n")


