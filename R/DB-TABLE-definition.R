#' Create a database table object
#'
#' This function is used to initialise a database table
#' @param ... parameters are passed to \code{\link{data.frame}}
#' @param version Can be used to track changes to table schema
#' @return A database table object
#' @export
jms.database.table <- function(...,validator=NULL,version=1) {
  log.info('Creating database table')
  as.jms.database.table(data.frame(...),validator,version=1)
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
as.jms.database.table <- function(x,validator,version) UseMethod("as.jms.database.table")

#' @export
as.jms.database.table.default <- function(x,validator,version) {
  stop("Unable to convert this class")
}

#' @export
as.jms.database.table.data.frame <- function(x,validator=NULL,version=1) {
  attr(x,'.path')<-''
  attr(x,'.modTime')<-.POSIXct(0)
  attr(x,'.hasChanged')<-TRUE
  attr(x, "class") <- c("jms.database.table", "data.frame")
  attr(x,'.validator')<-validator
  attr(x,'version')<-version
  return(x)
}
