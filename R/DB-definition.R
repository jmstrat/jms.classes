#' Create a database  object
#'
#' This function is used to initialise a database
#' @details A database object holds \code{\link{jms.database.table}} objects
#' @param path Path to the directory containing the database on disk
#' @return Database object
#' @export
jms.database <- function(path=NULL) {
  log.info('Creating new database with path: %s',path)
  self<-new.env()
  self$.path=path
  self$.table_names=c()
  self$.lockfile=paste0(self$.path,'/lock.lock')
  self$.modTime=.POSIXct(0)
  self$.tableHashes=c()
  self$.tablePaths=c()
  self$.hasChanged=TRUE
  as.jms.database(self)
}

#' Check if an object is a jms.database
#'
#' @param x The object to be tested
#' @return TRUE / FALSE
#' @export
is.jms.database <- function(x) {
  return(inherits(x,"jms.database"))
}

#' Convert an object into a jms.database
#'
#' @param x The object to be converted
#' @return The converted object
#' @export
as.jms.database <- function(x) UseMethod("as.jms.database")

#' @export
as.jms.database.default <- function(x) {
  tryCatch(as.jms.database(as.environment(x)),error = function(e) stop("Unable to convert this class"))
}

#' @export
as.jms.database.environment <- function(x) {
  attr(x, "class") <- c("jms.database", "environment")
  return(x)
}
