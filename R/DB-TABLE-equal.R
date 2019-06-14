#' @export
all.equal.jms.database.table <- function(target, current, ...) {
  load(target)
  load(current)
  NextMethod()
}

#' @export
`==.jms.database.table` <- function(e1, e2) {
  load(e1)
  load(e2)
  NextMethod()
}
