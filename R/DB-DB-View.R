#' @export
View.jms.database <- function(x,...) {
  tables<-mget(x$.table_names,envir=x)
  View(lapply(tables,as.data.frame),...,title=paste(deparse(substitute(x))[1]))
}
