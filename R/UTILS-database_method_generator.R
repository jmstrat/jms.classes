database.method.generator <- function(self, overwrite=FALSE) {
  function(name, func) {
    if (exists(name, envir=self) && !overwrite) {
      return()
    }
    f <- formals(func)
    nf <- names(f)
    if ("database" %in% nf) {
      f[["database"]] <- NULL
      f[["database"]] <- self
    } else if ("self" %in% nf) {
      f[["self"]] <- NULL
      f[["self"]] <- self
    } else if ("x" %in% nf) {
      f[["x"]] <- NULL
      f[["x"]] <- self
    } else {
      stop("function is not a database method")
    }
    formals(func) <- f
    self[[name]] <- func
  }
}

database.var.generator <- function(self, overwrite=FALSE) {
  function(name, value) {
    if (exists(name, envir=self) && !overwrite) {
      return()
    }
    self[[name]] <- value
  }
}
