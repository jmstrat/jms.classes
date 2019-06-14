## make an S3 generic for save
#' @export
save <- function(x, ...) UseMethod("save")
## take the usual definition of save,
## and set it to be the default method
#' @export
save.default <- base::save
formals(save.default) <- c(formals(save.default), alist(...=))

## make an S3 generic for load
#' @export
load <- function(x, ...) UseMethod("load")
## take the usual definition of load,
## and set it to be the default method
#' @export
load.default <- base::load
formals(load.default) <- c(formals(load.default), alist(...=))
