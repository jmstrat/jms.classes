#' @export
dimnames.jms.database.table <- function(x) dimnames(x$.table)

#' @export
`dimnames<-.jms.database.table` <- function(x, value) `dimnames<-`(x$.table, value)

#' @export
dim.jms.database.table <- function(x) dim(x$.table)
