#' Add a table to a database
#'
#' These functions are used to add or get a \code{\link{jms.database.table}} to or from a database
#' @param x The database
#' @param name The name of the table
#' @param value The table
#' @return Database or table object
#' @examples
#' \dontrun{
#' database <- jms.database("<path>")
#' database[["mytable"]] <- jms.database.table(...)
#' table <- database[["mytable"]]
#' }
#' @export
#' @rdname jms.database.add_get
`[[<-.jms.database` <- function(x, name, value) {
  if (startsWith(name, ".")) {
    NextMethod()
    return(x)
  }
  # Remove a table
  if (is.null(value)) {
    x$.removeTable(name)
  } else {
    x$.addTable(name, value)
  }
  x$.saveDatabase()
  return(x)
}

#' @export
#' @rdname jms.database.add_get
`[[.jms.database` <- function(x, name) {
  if (startsWith(name, ".")) {
    return(get(name, envir=x))
  }
  # Check for new tables
  x$.loadDatabase()
  # Find table
  x$.getTable(name)
}

### EXTRA METHODS:

#' @export
#' @rdname jms.database.add_get
`[<-.jms.database` <- function(x, name, value) {
  x[[name]] <- value
  return(x)
}
#' @export
#' @rdname jms.database.add_get
`$<-.jms.database` <- function(x, name, value) {
  x[[name]] <- value
  return(x)
}

#' @export
#' @rdname jms.database.add_get
`[.jms.database` <- function(x, name, ...) {
  `[[`(x, name, ...)
}
#' @export
#' @rdname jms.database.add_get
`$.jms.database` <- function(x, name, ...) {
  `[[`(x, name, ...)
}

#' @export
#' @rdname jms.database.add_get
`+.jms.database` <- function(e1, e2) {
  # TODO: get name, call x[[name]] <- value
  stop()
}
#' @export
#' @rdname jms.database.add_get
`-.jms.database` <- function(e1, e2) {
  # TODO: get name, call x[[name]] <- NULL
  stop()
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
unlock.jms.database <- function(x, force=FALSE) {
  log.info("Unlocking database")
  x$.unlockDatabase(force)
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
  tabNames <- x$.table_names
  tables <- lapply(tabNames, function(tab) as.data.frame(x$.getTable(tab)))
  names(tables) <- tabNames
  View(tables, ..., title=paste(deparse(substitute(x))[1]))
}
