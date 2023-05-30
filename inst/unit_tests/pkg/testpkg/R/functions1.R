f1 <- function(y) {
  ff1 <- function(g = y) {
    g + y
  }
  ff1(g + y)
}

f2 <- function(y) {
  ff2 <- function(x = z) {
    x + y
  }
  ff2(y + G)
}

f3 <- function(y) {
  ff3 <- function(x) {
    x + z
  }
  y
}

f4 <- function(y) {
  requireNamespace(g)
  pkg <- "utils"
  requireNamespace(quietly = TRUE, p = pkg)
  head(y)
  pkg <- "checkglobals"
  attachNamespace(depends = NULL, pkg, NULL)
  attachNamespace(getNamespace(pkg))
  library(quietly = TRUE, "stats")
  median(y)
  "median"(y)
  pkg <- "grid"
  library(pkg, character.only = TRUE)
  is.unit(structure("cm", class = "unit"))
}

f6 <- function(y) {
  x1 <- x2 <- y
  x3 = y
  x4 <<- y
  assign(value = y, x = "x5")
  delayedAssign(val = y, x = "x6")
  x1; x2; x3; x4; x5; x6
}

f7 <- function(y) {
  methods::setGeneric("ff6", function(x) standardGeneric("ff6"))
  methods::setMethod("ff6", signature = "ANY", function(x) x)
  ff6(y)
}

f8 <- function(y) {
  {
    for(x in 1) {
      x
    }
  }
}

f9 <- function(y) {
  utils::head(y)
  `::`("utils", "head")(y)
  get("head")(y)
  utils:::tail(y)
  methods::getMethod("ff6", signature = "ANY")
  getMethod("ff6", signature = "ANY")
  (stats::approxfun(0:1, 0:1))(y)
  stats::coef(y)
  stats4::coef(y)
  approxfun(0:1, 0:1)(y)
  list(h = utils::head)$h(y)
  list("function" = utils::head)$`function`(y)
  tanh^2(y)
}

f10 <- function(y) {
  local(g <- function(x) x)
  g
}

f11 <- function(y) {
  do.call(args = list(y), what = "g")
  lapply(FUN = "g", 1, n = 1)
  lapply(X = 1, "g")
  Map("g", 1, n = 1)
  stats::aggregate(x ~ ., data = y, FUN = "g")
  pvec(1, "g")
  pvec(v = 1, mc.cores = 1L)
  parallel::pvec(mc.cores = 1L, 1, "g")

}

f12 <- function(y) {
  .Call("Cfun", NULL)
}

f13 <- function(y) {
  substitute(q1)
  quote(q2)
  bquote(q3)
  ~q4
  Quote(q5)
}

f14 <- function(y) {
  g(1, y)
}

f15 <- function(...) {
  ...elt(1)
  ..1
  ..2
  g
}

f16a <- function(y) {
  `set<-` <- function(var, value) {
    var <- value; var
  }
}

`f16b<-` <- function(var, value) {
  var <- value; var
}

f17 <- function(y) {
  dataset
  sysdata
}

f18 <- R6::R6Class("f18",
                   public = list(
                     initialize = function(...) self$f(...),
                     f = function(...) private$p
                   ),
                   private = list(
                     p = list()
                   )
)
