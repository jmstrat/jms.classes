## make an S3 generic for View
#' @export
View <- function(...) UseMethod("View")
## take the usual definition of View,
## and set it to be the default method
## this mget must be at run time to capture the RStudio override (i.e. wrapped in function)
#' @export
View.default <- function(...) mget("View", as.environment("package:utils"))[[1]](...)
