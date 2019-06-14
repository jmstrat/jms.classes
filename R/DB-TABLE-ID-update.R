#' Update a row in the table
#'
#' @export
`[<-.jms.database.table.id` <- function(x, i=NULL, j=NULL, value) {
  if (!length(j)) {
    j <- names(value)
  }
  if (is.null(j)) {
    j <- colnames(x)
  } else {
    j <- c("id", j)
  }
  ids <- x[, "id"]
  if (all(is.na(ids))) ids <- numeric()
  if (missing(i)) {
    id <- max(0, ids, na.rm=T) + 1 # 0 to account for empty table!
    i <- length(ids) + 1 # Index =!= id
    log.info("Adding a new table row with id %s", id)
  } else {
    if (any(i < 0)) stop("Use x[-id] to remove ids")
    s <- if (length(i) == 1) "" else "s"
    log.info("Updating table row%s with id%s %s", s, s, i)
    id <- i
    i <- which(ids %in% id)
  }
  value <- if (is.null(names(value))) append(list(id), value, after=1) else append(list(id=id), value, after=1)
  if (!length(i)) stop("Invalid ID supplied")
  log.debug(
    "i=[%s], j=[%s], value=[%s]",
    paste0(i, collapse=","),
    paste0(j, collapse=","),
    paste0(value, collapse=",")
  )
  `[<-.jms.database.table`(x, i, j, value)
}
