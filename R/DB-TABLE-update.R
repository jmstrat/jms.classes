#' @export
`[<-.jms.database.table` <- function(x, i, j, value) {
  if (missing(i)) i <- T
  if (missing(j)) j <- T
  if (any(i < 0 || j < 0)) stop("Negative subsetting not allowed for this class")
  log.info(
    "Updating {[%s],[%s]} with {%s}",
    paste0(i, collapse=","),
    paste0(j, collapse=","),
    paste0(value, collapse=",")
  )
  # Validate the new value
  validator <- x$.validator
  table <- x$.table
  n <- colnames(table)
  names(n) <- n
  log.debug("Available columns: [%s]", paste0(n, collapse=","))
  tovalidate <- n[j]
  log.debug("Columns supplied:  [%s]", paste0(tovalidate, collapse=","))
  # Add any missing names, reorder etc.
  value <- match.names(tovalidate, value)
  # We can now assume that value is a fully named list ordered as tovalidate
  if (!is.null(validator)) {
    log.info("Validating new values")
    if (any(is.na(tovalidate))) stop("Invalid options supplied")
    l <- length(tovalidate)
    validate <- as.data.frame(matrix(value, nrow=length(value) / l, ncol=l), stringsAsFactors=FALSE)
    log.debug("New values matrix: %s", toString(validate))
    if (nrow(validate) == 1) validate <- unlist(validate, FALSE)
    validatelist <- as.list(validate)
    names(validatelist) <- tovalidate
    log.debug("Argument for validator: %s", paste(names(validatelist), validatelist, sep="=", collapse=","))
    value <- do.call(validator, as.list(validatelist))[j]
    log.debug("Values returned: [%s]", paste0(value, collapse=","))
    # Now we need to restore the original dimensions
    # value<-as.data.frame(value,stringsAsFactors=FALSE)
    log.info(
      "Validated update {[%s],[%s]} with {%s}",
      paste0(i, collapse=","),
      paste0(j, collapse=","),
      paste0(value, collapse=",")
    )
  }
  # Replace invalid values with NA
  value[is.null(value)] <- NA
  idx <- !(sapply(value, length))
  value[idx] <- NA
  # Do the update
  df <- `[<-.data.frame`(table, i, j, value)
  # Mark the table as modified
  x$.hasChanged <- TRUE
  # Update rownames
  rownames(df) <- 1:nrow(df)
  # Make the change
  x$.table <- df
  return(x)
}
#' @export
`[[<-.jms.database.table` <- function(x, ...) {
  `[<-`(x, ...)
}
#' @export
`$<-.jms.database.table` <- function(x, name, value, ...) {
  if (name == ".table") {
    assign(name, value, envir=x)
    save(x)
  }
  if (startsWith(name, ".")) {
    assign(name, value, envir=x)
    return(x)
  }
  stop("$ operator invalid for database objects")
}
