#' @export
View.jms.database.table <- function(x,...) View(x$.table,...,title=paste(deparse(substitute(x))[1]))
