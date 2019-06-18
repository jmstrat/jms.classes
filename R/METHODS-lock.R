#' Lock a database to prevent editing
#'
#' @param x The database to lock
#' @export
#' @rdname jms.database.lock
lock <- function(x) UseMethod("lock")

#' @export
lock.default <- function(x) {
  stop("Unable to lock this object")
}

#' @export
#' @rdname jms.database.lock
unlock <- function(x, force=FALSE) UseMethod("unlock")

#' @export
unlock.default <- function(x, force=FALSE) {
  stop("Unable to unlock this object")
}
