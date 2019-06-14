#' Get an element from the table
#'
#' @export
`[.jms.database.table.id` <- function(x, i=T, j=T) {
  if (!length(i)) i <- TRUE
  if (!length(j)) j <- TRUE
  ids <- x$.table[, "id"]
  inum <- is.numeric(i)
  iltz <- i < 0
  if (any(inum)) {
    iids <- which(ids %in% abs(i[inum]))
    if (!length(iids)) stop("ID not found")
    i[inum] <- iids
  }
  if (any(iltz)) i[iltz] <- i[iltz] * -1
  `[.jms.database.table`(x, i, j)
}
