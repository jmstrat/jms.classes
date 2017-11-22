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

#' @method as.jms.database.table default
#' @export
as.jms.database.table.default <- function(x,validator,version) {
  stop("Unable to convert this class")
}

#' @method as.jms.database.table data.frame
#' @export
as.jms.database.table.data.frame <- function(x,validator=NULL,version=1) {
  tableEnv <- new.env()
  tableEnv$.hasChanged<-TRUE
  tableEnv$.validator<-validator
  tableEnv$.version<-version
  tableEnv$.table<-x
  tableEnv$.name<-''
  attr(tableEnv, "class") <- c("jms.database.table", "environment")
  return(tableEnv)
}

#' @method as.data.frame jms.database.table
#' @export
as.data.frame.jms.database.table <- function(x) {
  return(x$.table)
}
