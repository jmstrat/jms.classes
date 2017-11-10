#' Get an element from the table
#'
#' @export
`[.jms.database.table` <- function(x,name,...) {
  if(is.stale(x)) {
    x<-load(x)
    attr(x,'.database')[[attr(x,'.name')]]<-x
  }
  NextMethod()
}
#' @export
`[[.jms.database.table`<-function(x,name,...) {
  if(is.stale(x)) {
    x<-load(x)
    attr(x,'.database')[[attr(x,'.name')]]<-x
  }
  NextMethod()
}
#' @export
`$.jms.database.table`<-function(x,name,...) {
  if(is.stale(x)) {
    x<-load(x)
    attr(x,'.database')[[attr(x,'.name')]]<-x
  }
  NextMethod()
}
