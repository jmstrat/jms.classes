#' @export
save.jms.database.table <- function(x,...) {
  if(inherits(parent.env(x),'jms.database')) {
    log.debug('Preparing to save table')
    parent.env(x)$.addTable(x$.name,x)
  }
  return(x)
}
#' @export
load.jms.database.table <- function(x,...) {
  if(inherits(parent.env(x),'jms.database')) {
    log.debug('Preparing to load table')
    parent.env(x)$.getTable(x$.name)
  } else {
    x
  }
}
