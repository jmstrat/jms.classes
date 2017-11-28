#' @export
View.jms.database.table <- function(x,...) View(as.data.frame(x),...,title=paste(deparse(substitute(x))[1]))
