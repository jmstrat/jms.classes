#' Create a database  object
#'
#' This function is used to initialise a database
#' @details A database object holds \code{\link{jms.database.table}} objects
#' @param path Path to the directory containing the database on disk
#' @return Database object
#' @export
jms.database <- function(path=NULL) {
  log.info("Creating new database with path: %s", path)
  self <- new.env(parent=emptyenv())
  self$.path <- path
  self$.self <- self
  as.jms.database(self)
}

#' Check if an object is a jms.database
#'
#' @param x The object to be tested
#' @return TRUE / FALSE
#' @export
is.jms.database <- function(x) {
  return(inherits(x, "jms.database"))
}

#' Convert an object into a jms.database
#'
#' @param x The object to be converted
#' @return The converted object
#' @export
as.jms.database <- function(x) UseMethod("as.jms.database")

#' @export
as.jms.database.default <- function(x) {
  tryCatch(as.jms.database(as.environment(x)), error=function(e) stop("Unable to convert this class"))
}

#' @export
as.jms.database.environment <- function(x) {
  self <- get0(".self", envir=x, ifnotfound=x)
  database.method <- database.method.generator(self)
  database.var <- database.var.generator(self)

  ## Values:
  database.var(".path", NULL)
  database.var(".table_names", c())
  database.var(".hash", "")
  database.var(".tableHashes", list())
  database.var(".hasChanged", TRUE)
  ## Methods:
  # Table
  database.method(".addTable", addTable)
  database.method(".getTable", getTable)
  database.method(".removeTable", removeTable)
  # Save_Load
  database.method(".saveTable", saveTable)
  database.method(".loadTable", loadTable)
  database.method(".saveDatabase", saveDatabase)
  database.method(".loadDatabase", loadDatabase)
  # Paths
  database.method(".getPathForTable", getPathForTable)
  database.method(".getPathForDatabase", getPathForDatabase)
  # Hash
  database.method(".getHashForTable", getHashForTable)
  database.method(".getHashForDatabase", getHashForDatabase)
  database.method(".checkHashForTable", checkHashForTable)
  database.method(".checkHashForDatabase", checkHashForDatabase)
  database.method(".updateHashForTable", updateHashForTable)
  database.method(".updateHashForDatabase", updateHashForDatabase)
  # Lock
  database.method(".lockTable", lockTable)
  database.method(".unlockTable", unlockTable)
  database.method(".lockDatabase", lockDatabase)
  database.method(".unlockDatabase", unlockDatabase)
  # Plot
  database.method(".plotDatabase", plotDatabase)

  attr(x, "class") <- c("jms.database", "environment")
  return(x)
}

#' @export
save.jms.database <- function(x, ...) {
  x$.saveDatabase()
}

#' @export
load.jms.database <- function(x, ...) {
  x$.loadDatabase()
}

#' @export
lock.jms.database <- function(x) {
  log.info("Locking database")
  x$.lockDatabase()
}

#' @export
unlock.jms.database <- function(x) {
  log.info("Unlocking database")
  x$.unlockDatabase()
}

#' @export
is.stale.jms.database <- function(x) {
  !x$.checkHashForDatabase()
}

#' @export
plot.jms.database <- function(x, ...) {
  x$.plotDatabase()
}

#' @export
print.jms.database <- function(x, ...) {
  load(x)
  cat("jms.database with the following tables:", paste0(x$.table_names, collapse=", "))
}

#' @export
View.jms.database <- function(x, ...) {
  tables <- mget(x$.table_names, envir=x)
  View(lapply(tables, as.data.frame), ..., title=paste(deparse(substitute(x))[1]))
}

#' @export
database2reference <- function(database) {
  if (!is.jms.database(database)) stop("database must be a jms.database")
  # Make a new environment whose methods will continue to point to the old
  # database in effect this gives us a 2nd pointer to the environment but with
  # a differnent class. Simply updating the class would change this globally
  db <- new.env(parent=database)
  class(db) <- c("jms.database", "environment")
  db
}
