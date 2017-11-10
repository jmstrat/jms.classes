#' Create a database table object
#'
#' This function is used to initialise a database table
#' @param ... parameters are passed to \code{\link{data.frame}}
#' @return A database table object
#' @export
#' @keywords internal
jms.database.table <- function(...) {
  log.info('Creating database table')
  as.jms.database.table(data.frame(...))
}

#' Check if an object is a jms.database.table
#'
#' @param x The object to be tested
#' @return TRUE / FALSE
#' @export
is.jms.database.table <- function(x) {
  return(inherits(x,"jms.database.table"))
}

#' Convert an object into a jms.database.table
#'
#' @param x The object to be converted
#' @return The converted object
#' @export
as.jms.database.table <- function(x) UseMethod("as.jms.database.table")

#' @export
as.jms.database.table.default <- function(x) {
  stop("Unable to convert this class")
}

#' @export
as.jms.database.table.data.frame <- function(x) {
  attr(x,'.path')<-''
  attr(x,'.modTime')<-.POSIXct(0)
  attr(x,'.hasChanged')<-TRUE
  attr(x, "class") <- c("jms.database.table", "data.frame")
  return(x)
}
