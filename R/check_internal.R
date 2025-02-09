.check_internal <- function(expr, is_pkg = FALSE, include_compiled = FALSE, skip_globals = NULL, verbose = FALSE) {

  ## initialize environments
  env0 <- list2env(
    c(
      ## skipped symbols that may/may not exist
      structure(
        vector(mode = "list", length = 14L),
        names = c("last.dump", "last.warning", ".Last.value", ".Random.seed", ".Traceback",
                  "shell", "shell.exec", "Sys.junction", ".GenericDefEnv", ".GenericCallEnv",
                  ".Method", ".Group", ".Class", ".Generic")
      ),
      if(length(skip_globals))
        structure(
          vector(mode = "list", length = length(skip_globals)),
          names = skip_globals
        ),
      ## variables for internal use
      list(
        ".__unknown__" = NULL,
        ".__closure__" = TRUE,
        ".__srcref__" = attr(expr, "srcref")
      )
    ),
    parent = baseenv(),
    hash = TRUE
  )
  attr(expr, "enclos") <- env0

  envi <- list2env(list(".__pkgs__" = NULL), parent = emptyenv(), hash = TRUE)
  envg <- new.env(hash = TRUE, parent = emptyenv())
  srcrefg <- new.env(hash = TRUE, parent = emptyenv())
  srcrefi <- new.env(hash = TRUE, parent = emptyenv())

  ## walk expression
  .Call(
    walk_expr,
    expr, env0, envi, envg, environment(), srcrefi, srcrefg,
    isTRUE(is_pkg), isTRUE(include_compiled), isTRUE(verbose)
  )

  ## filter globals
  rm(list = intersect(
    objects(envg, all.names = TRUE, sorted = FALSE),
    objects(env0, all.names = TRUE, sorted = FALSE)
  ), envir = envg, inherits = FALSE)

  ## source references
  srcrefg <- as.list(srcrefg, all.names = TRUE)
  srcrefg <- srcrefg[is.element(names(srcrefg), objects(envg, all.names = TRUE, sorted = FALSE))]
  srcrefi <- as.list(srcrefi, all.names = TRUE)
  srcrefi <- srcrefi[is.element(names(srcrefi), objects(envi, all.names = TRUE, sorted = FALSE))]

  return(list(globals = envg, imports = envi, srcrefg = srcrefg, srcrefi = srcrefi))

}




