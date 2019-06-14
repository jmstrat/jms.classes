#' Checks whether a database has been updated on disk, but not in memory
#'
#' @param x The database to check
#' @export
is.stale <- function(x) UseMethod("is.stale")
#' @export
is.stale.default <- function(x) {
  stop("Unable to check state for this object")
}
