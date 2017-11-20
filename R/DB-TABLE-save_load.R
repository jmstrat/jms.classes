#' @export
save.jms.database.table <- function(x,...) {
  if(inherits(parent.env(x),'jms.database')) {
    log.info('Preparing to save table')
    parent.env(x)[[x$.name]]<-x
  }
  return(x)
}
#' @export
load.jms.database.table <- function(x,...) {
  if(inherits(parent.env(x),'jms.database')) {
    log.info('Preparing to load table')
    parent.env(x)[[x$.name]]
  } else {
    x
  }
}
