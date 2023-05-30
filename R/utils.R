.find_pkgs <- function(pkgs) {
  exist <- nzchar(vapply(pkgs, function(pkg) system.file(package = pkg[[1]]), character(1)))
  if(is.list(pkgs)) {
    names(exist) <- vapply(pkgs, `[[`, character(1), 1)
  } else {
    names(exist) <- pkgs
  }
  return(exist)
}

`%||%` <- function(l, r) if (is.null(l)) r else l
